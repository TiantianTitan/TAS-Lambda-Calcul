# Interpréteur du Calcul Lambda

---

## **Introduction**

Ce projet est un interpréteur du calcul lambda implémenté en OCaml. Il supporte l'évaluation des expressions lambda, les opérations arithmétiques, les branches conditionnelles, ainsi que les abstractions et applications.

---

## **Instructions d'utilisation**

1. **Compiler le projet :**
   Exécutez la commande suivante pour compiler tous les fichiers nécessaires :
   ```bash
   make all
2. **Lancer le REPL : Après compilation, lancez le REPL**(Read-Eval-Print Loop) avec la commande :

bash:
./exec/main

3. **Quitter le REPL : Tapez exit pour quitter l'interpréteur.**

4. **Exécuter les tests : Les tests se trouvent dans les fichiers**
test_lambda_eval.ml et test_lambda_type.ml. Pour les exécuter :

bash:
make test

## **Cas de test**
Voici quelques exemples de commandes que vous pouvez entrer dans le REPL et leurs sorties correspondantes :

1. **Entier**
text

> 1
Expression : 1
Évaluation : 1
Résultat : 1

2. **Variable**
text
> x
Expression : x
Erreur : Variable non définie : x

3. **Abstraction lambda**
text

> λx. x
Expression : λx. x
Évaluation : λx. x
Résultat : λx. x

4. **Branche conditionnelle (condition vraie)**
text

> if 0 = 0 then 1 else 2
Expression : if 0 = 0 then 1 else 2
Évaluation : if 0 = 0 then 1 else 2
Étape intermédiaire : 1
Résultat : 1

5. **Branche conditionnelle (condition fausse)**
text

> if 1 = 0 then 3 else 4
Expression : if 1 = 0 then 3 else 4
Évaluation : if 1 = 0 then 3 else 4
Étape intermédiaire : 4
Résultat : 4

6. **Abstraction lambda avec arithmétique**
text

> λx. x + 1
Expression : λx. (x + 1)
Évaluation : λx. (x + 1)
Résultat : λx. (x + 1)

7. **Abstraction lambda avec déclaration de variable**
text

> λy. let x = y + 1 in x * 2
Expression : λy. let x = (y + 1) in (x * 2)
Évaluation : λy. let x = (y + 1) in (x * 2)
Résultat : λy. let x = (y + 1) in (x * 2)


## **Notes importantes**

Syntaxe des expressions :

- Les abstractions lambda peuvent être écrites avec λ ou \.
- Les variables, entiers et opérations arithmétiques suivent une syntaxe classique.
- Les branches conditionnelles utilisent if <condition> = <valeur> then <expr1> else <expr2>.
- Les déclarations de variables utilisent let <var> = <valeur> in <expr>.



## **Compilation et exécution**
- Pour compiler et exécuter le projet :

bash:
make all
./exec/main

- Pour exécuter les tests unitaires :

bash:
make test