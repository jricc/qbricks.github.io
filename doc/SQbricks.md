# SQbricks : documentation technique

[Français](SQbricks.md) | [English](SQbricks.en.md)

## Objectif

Ce document complète le [`README.md`](../README.md). Le README présente
l'installation et les commandes principales ; ce document décrit
l'architecture de SQbricks, les contrats stables de ses sous-systèmes et leurs
limites connues.

Les commentaires `odoc`, les interfaces `.mli` et les tests restent la source
de vérité pour le détail fonction par fonction. Les preuves, études de commits
et mesures expérimentales ne font pas partie de cette documentation publique.

## Vue d'ensemble

SQbricks est un prototype de recherche pour la vérification de circuits
quantiques hybrides. Un programme peut combiner des portes quantiques, des
mesures, des initialisations et du contrôle classique.

Le workflow principal comporte deux parties :

- **SQbricks-Lift (SQL)** transforme un circuit hybride en déplaçant les
  mesures et en isolant une représentation unitaire exploitable ;
- **SQbricks-Verif (SQV)** exécute symboliquement deux programmes unitaires,
  réduit leurs path-sums et cherche à établir leur équivalence.

Le coeur utilise `Program.t` comme représentation des circuits. Les indices de
qubits et de bits classiques y sont des entiers plats. L'exécution symbolique
produit un `Path_sum.t`, puis `Reduction_algorithm` applique les règles de
réduction avant la décision d'équivalence.

Les deux algorithmes d'équivalence sont :

- `Equiv.seq`, qui compose le premier circuit avec l'inverse du second ;
- `Equiv.parallel`, qui exécute et réduit les deux circuits séparément avant de
  comparer leurs path-sums.

`Equiv.seq` exige donc que le circuit inversé soit réversible. Le mode
`Parallel` convient aussi aux comparaisons où cette inversion n'est pas
possible.

## Provenance et références

SQbricks a d'abord été développé dans le
[dépôt Qbricks](https://github.com/Qbricks/qbricks.github.io), sous
`Artifacts/SQbricks/SQbricks`. Le dépôt actuel place SQbricks à sa racine tout
en conservant l'historique du sous-projet, ses mentions de copyright et sa
licence LGPL 2.1.

Les références principales sont :

- Jérôme Ricciardi, Sébastien Bardin, Christophe Chareton et Benoît Valiron,
  [*Quantum Circuit Equivalence Checking: A Tractable Bridge From Unitary to
  Hybrid Circuits*](https://arxiv.org/abs/2511.22523), 2025 ;
- Jérôme Ricciardi,
  [*Practical verification of quantum circuit
  transformations*](https://theses.hal.science/tel-05681895v1/document),
  thèse de doctorat, 2026.

Le prototype d'inspection utilise Quantikz2. Sa référence est Alastair Kay,
[*Tutorial on the Quantikz Package*](https://arxiv.org/abs/1809.03842).
[`CITATION.cff`](../CITATION.cff) fournit les métadonnées de citation du projet.

## Benchmarks de non-régression

SQbricks fournit trois niveaux de benchmark qui n'appellent pas les outils de
vérification externes du benchmark historique :

| Niveau | Objectif | Commande principale |
| --- | --- | --- |
| Light | garde-fou court fonctionnel et de performance | `make regression-light-check` |
| Large sélectionné | cas plus coûteux proches des frontières connues | `make regression-large-check` |
| Long | campagne SQbricks-only sur les familles historiques | `make benchmarks-sqbricks` |

Les familles `qiskit-hybrid` et `owm-vs-qiskit` utilisent encore Qiskit pour
produire un circuit transformé. Qiskit n'est pas utilisé comme vérificateur et
n'apparaît pas comme résultat de vérification dans les CSV SQbricks-only.

### Benchmark light

Les points d'entrée sont :

| Commande | Rôle |
| --- | --- |
| `make regression-light` | exécuter les cas et écrire le CSV courant |
| `make regression-light-baseline` | régénérer `benchmarks/baseline/light.csv` |
| `make regression-light-check` | comparer une exécution à cette baseline |
| `make tests_regression_light` | tester le runner avec un faux exécutable SQbricks |

Les cas sont décrits dans :

- `scripts/paths/light/pairs.csv` pour les comparaisons directes ;
- `scripts/paths/light/transforms.csv` pour les circuits transformés avant la
  comparaison.

Le manifest est l'oracle fonctionnel. Chaque mode activé doit conserver son
statut attendu : `EQ`, `NE`, `NC` ou un statut explicite d'échec. Une sortie
réussie mais vide ou inconnue devient `UNEXPECTED_OUTPUT` et fait échouer le
check.

Les lignes marquées pour le suivi de performance sont exécutées pendant trois
rounds par défaut. Le meilleur temps observé est comparé à la baseline. Une
régression n'est signalée que si le seuil relatif
`SQBRICKS_LIGHT_PERF_THRESHOLD` et le seuil absolu
`SQBRICKS_LIGHT_MIN_SLOWDOWN_SECONDS` sont tous les deux dépassés. Les temps
très courts, inférieurs à `SQBRICKS_LIGHT_MIN_PERF_SECONDS`, ne donnent pas une
mesure exploitable.

La performance dépend de la machine. Une baseline doit donc être régénérée
après un changement intentionnel du manifest, de la politique de mesure ou de
l'environnement de référence.

### Régression large sélectionnée

La régression large réutilise `scripts/benchmarks-sqbricks.sh` avec les listes
plus courtes de `scripts/paths/regression-large/`. Ses baselines sont stockées
par famille dans `benchmarks/baseline/regression-large/`.

Le check vérifie :

- que les lignes de baseline attendues sont toujours présentes ;
- qu'aucune capacité fonctionnelle connue n'est perdue ;
- qu'un temps ne dépasse pas simultanément les seuils relatif et absolu.

Une amélioration fonctionnelle est affichée sans faire échouer le check. Pour
les familles ordonnées par taille, la sélection conserve plusieurs cas autour
de la frontière de ressources afin qu'une fluctuation du plus gros cas ne
masque pas toute la famille.

### Benchmark long

`scripts/benchmarks-sqbricks.sh` exécute une famille avec :

```text
make benchmark-sqbricks TYPE=owm
```

`make benchmarks-sqbricks` parcourt toutes les familles de `LONG_TYPES`.
Chaque famille écrit un CSV séparé dans `benchmarks/result/<mois>/`.

Le runner applique par défaut une limite de 600 secondes de CPU et une limite
mémoire de 6 Gio environ à chaque processus. Elles sont configurables avec
`SQBRICKS_LONG_TIMEOUT` et `SQBRICKS_LONG_MEMORY_KB`.

Dans une série ordonnée, Sequence et Parallel sont suivis séparément. Après un
timeout ou un dépassement mémoire, les cas plus grands ne sont sautés que pour
le mode concerné et reçoivent `SKIP_AFTER_RESOURCE_FAILURE`. Un échec de
ressource pendant une conversion arrête les deux modes de cette série, mais pas
les autres familles.

Les barres de progression sont écrites sur `stderr`, tandis que les données CSV
sont écrites sur `stdout`. Elles ne contaminent donc pas les fichiers de
résultats redirigés.

## Parser et export OpenQASM

Le parser OpenQASM traduit les circuits vers `Program.t`. Plusieurs registres
nommés sont aplatis dans des espaces d'indices quantiques et classiques
distincts. Chaque déclaration reçoit un offset global ; `q[i]` devient ainsi
`offset(q) + i`.

Une condition OpenQASM `if (c == n)` conserve la taille et l'offset du registre
`c`. Le bit d'indice zéro est le bit de poids faible. La traduction distingue
les bits attendus à zéro de ceux attendus à un afin de préserver exactement la
condition. Une valeur trop large pour le registre ou pour la représentation
entière de SQbricks est rejetée explicitement.

Certaines entrées mal formées issues de bibliothèques de benchmark sont
acceptées avec un warning afin de ne pas modifier les jeux de données externes.
Cette tolérance ne rend pas le circuit bien formé et les étapes suivantes
peuvent toujours le refuser.

Limites de compatibilité actuelles :

- `include "...";` est accepté mais le fichier indiqué n'est pas chargé ;
- `OPENQASM 3.0;` n'autorise que le sous-ensemble legacy déjà compris par le
  parser, pas OpenQASM 3 en général ;
- `barrier ...;` est traité comme un no-op jusqu'au prochain point-virgule ;
- les dénominateurs d'angles doivent être des puissances de deux ;
- les portes inconnues ou les constructions sans sémantique SQbricks sont
  refusées.

Les canaux des points d'entrée fichier sont fermés après un succès comme après
une exception de parsing.

À l'export, `one_creg=true` regroupe les bits classiques sans supprimer le
registre quantique ni le circuit. Les applications à plusieurs cibles sont
séparées puis validées cible par cible. Une combinaison non supportée est
rejetée explicitement.

La sémantique `Program.Apply` accepte une liste arbitraire de contrôles. Pour le
sous-ensemble OpenQASM/OWM actuel, `Program.Macros.c3xdecomp` fournit une
décomposition exacte d'une porte X à trois contrôles, sans ancilla et sans phase
globale. Les portes H avec plusieurs contrôles restent exécutables
symboliquement mais n'ont pas encore de décomposition OpenQASM générale.

## Traduction en mesures différées

`To_deferred_measurement.to_deferred_measurements_result` transforme un
programme hybride en programme sans mesures intermédiaires. Elle retourne :

- le programme traduit ;
- les qubits initialisés ;
- les qubits mesurés ;
- ou une erreur typée.

Les bits classiques sont initialisés à zéro. Une mesure remplace cette valeur
par le qubit qui porte le résultat différé. `Not` inverse la condition, une
condition connue fausse est supprimée, et une condition partiellement mesurée
conserve uniquement les contrôles quantiques encore nécessaires.

Un bit classique peut être réutilisé : le contrôle suivant dépend de sa
dernière mesure. La liste des qubits déjà mesurés reste toutefois complète afin
qu'une réutilisation du bit n'efface pas l'historique quantique.

`InitQ` est accepté pour un qubit frais, notamment pour les ancillas introduites
par OWM. Réinitialiser un qubit déjà utilisé n'est pas encore supporté.

Les erreurs publiques sont :

- `InvalidClassicalBit` ;
- `InvalidQubitIndex` ;
- `ResetOfUsedQubitUnsupported` ;
- `MeasuredQubitUsedAfterMeasurement` ;
- `UnsupportedConditionalProgram`.

`to_deferred_measurements` reste le wrapper historique. Il conserve le même
triplet de retour, mais lève `Failure` lorsqu'une erreur typée est rencontrée.

## Vérification d'équivalence

### Métadonnées et bonne formation

Les listes `inputs`, `outputs` et `meas` décrivent la correspondance logique
entre les deux circuits. Leurs indices doivent être valides et leurs longueurs
compatibles. Les sorties mesurées sont comparées par position logique, pas par
égalité de leurs indices physiques.

Avant l'exécution symbolique, `Equiv` distingue :

- les métadonnées incompatibles, signalées par `NotEquivDiffInputs`,
  `NotEquivDiffOutputs`, `NotEquivDiffInputsOutputs` ou
  `ErrorInvalidQubitIndex` ;
- les constructions hybrides restantes, signalées par
  `ErrorCircuitNotUnitary` ;
- les applications de portes unitaires mal formées, signalées par
  `ErrorInvalidProgram`.

Pour `H`, `X` et `U1`, une cible est obligatoire, tous les indices doivent être
dans la largeur du circuit et les contrôles doivent être distincts des cibles.
Les angles effectifs de `GP` et `U1` doivent être dyadiques. Ils sont normalisés
modulo un au moment de l'exécution symbolique ; le `Program.t` d'origine n'est
pas réécrit.

### Path-sums et réduction

`Program.execution_result` produit un path-sum ou une erreur d'exécution typée.
`Reduction_algorithm.reduction_algorithm` applique ensuite les règles de
réduction. Une règle qui ne s'applique pas à un path-sum valide est distinguée
d'un path-sum mal formé ; ce dernier remonte jusqu'à `Equiv` sous la forme
`ErrorMalformedPathSum`.

Les comparaisons de qubits, monômes, polynômes, kets et path-sums utilisent des
fonctions `*_result` lorsqu'une incohérence de métadonnées doit être distinguée
d'une vraie inégalité.

Les variables d'entrée conservent leur indice. Les variables de chemin peuvent
être renommées bijectivement entre deux kets. La même bijection est ensuite
réutilisée pour comparer les phases ; une table incomplète est considérée
comme un path-sum mal formé.

La réduction par changement de variable reconnaît actuellement une forme
affine limitée : une variable de chemin XOR une constante et des variables
d'entrée, sans produit ni autre variable de chemin dans le décalage. La
substitution est appliquée à toute la phase et à tout le ket seulement si elle
isole davantage de composantes de sortie. Cette restriction est une limite de
l'implémentation actuelle, pas du changement de variable mathématique général.

## Prototype d'inspection

`scripts/inspect-sqbricks.sh` orchestre les commandes existantes pour inspecter
une comparaison entre deux fichiers QASM.

- `--mode auto` utilise le workflow automatique `-sq` ;
- `--mode manual` utilise `-sqv` avec les métadonnées explicites.

Les résultats sont écrits par défaut dans `_tmp/inspection/<timestamp>/`. Les
artefacts principaux sont :

- `report.txt`, résumé de l'exécution ;
- `commands.sh`, commandes rejouables ;
- `sqv.stdout` et `sqv.stderr`, trace complète ;
- les path-sums d'entrée et les path-sums finaux extraits ;
- des sources LaTeX et, lorsque possible, des PDF Quantikz2 pour les circuits
  et les path-sums.

L'extraction des path-sums dépend encore du format texte de debug. L'export de
circuits prend en charge un sous-ensemble simple d'OpenQASM 2 et limite la
taille des circuits pour éviter des PDF illisibles. Les seuils sont configurés
avec les variables `SQBRICKS_INSPECT_*` décrites dans le script.

La suite prévue est une interface graphique pour charger deux circuits,
modifier les métadonnées, choisir le mode automatique ou manuel, lancer SQV et
parcourir les artefacts produits.

## Où trouver les détails

- Interfaces publiques OCaml : fichiers `lib/*.mli`.
- Comportements validés : tests Alcotest dans `test/`.
- Runners et formats de benchmark : `scripts/benchmarks-light.sh`,
  `scripts/benchmarks-sqbricks.sh` et `scripts/check-regression-large.sh`.
- Planification du projet : [`ROADMAP.md`](../ROADMAP.md) et
  [`TODO.md`](../TODO.md).
