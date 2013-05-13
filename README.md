OCAML-ECC
=========

Projet: la cryptographie sur courbes elliptiques en OCaml

Ce dossier va héberger l'implémentation que je vais faire de la cryptographie sur courbes elliptiques que je vais réaliser en Ocaml.
Un autre dossier va contenir mes feuilles de calcul Maple, qui donne des résultats plus visuels, les garphes venant illustrer dans un premier temps le principe qui sous-tend la construction du groupe des points d'une courbe elliptique.
Le document opérationsmodulaires.ml va contenir les définitions caml qui vont permettre de faire les calculs dans Z/pZ en Caml, essentielement l'inversion des éléments  Z/pZ quand p est premier.
Le second document ecc.ml concerne les opérations cryptographiques (pour l'instant tout ne marche pas encore super bien, mais je corrige les bugs au fur et à mesure que je les comprends).
