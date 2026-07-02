(require 'package)
(package-initialize)

(setq column-number-mode t
      compilation-scroll-output t
      find-file-visit-truename t
      inhibit-startup-message t
      custom-file "~/.emacs.d/my-custom.el"
      vc-follow-symlinks t
      visible-bell 1
      truncate-lines t
      shell-file-name "bash"
      uniquify-buffer-name-style 'reverse
      ad-redefinition-action 'accept
      calendar-week-start-day 1
      select-enable-primary t
      select-enable-clipboard nil
      gc-cons-threshold 100000000
      dired-dwim-target t
      user-full-name "Maxime Rey"
      enable-remote-dir-locals t)

(setq-default indent-tabs-mode nil
              fill-column 78)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Any add to list for package-archives (to add marmalade or melpa) goes here
(add-to-list 'package-archives 
    '("MELPA" .
      "http://melpa.org/packages/"))

(global-auto-revert-mode)
(global-font-lock-mode t)
(show-paren-mode)
(blink-cursor-mode -1)
(evil-mode)
(ivy-mode)
(counsel-mode)
(savehist-mode)

(with-eval-after-load 'ivy
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))
        ivy-virtual-abbreviate 'full
        counsel-find-file-ignore-regexp "\\.go\\'"
        enable-recursive-minibuffers t
        recentf-max-saved-items nil))

_______________________________
Agis comme un développeur web senior spécialisé dans les expériences émotionnelles interactives.

Développe un projet complet prêt à être déployé sur GitHub Pages.

Le site n'est pas une application classique mais une expérience contemplative et poétique.

Le site est accessible via un tag NFC intégré dans un vêtement.

Il n'existe qu'un seul utilisateur.

L'expérience doit évoquer :

- la douceur
- la nostalgie
- le temps qui passe
- la croissance
- les souvenirs

STYLE VISUEL

Créer une direction artistique inspirée :

- des illustrations aquarelle
- des carnets de voyage
- des livres illustrés haut de gamme

Éviter tout aspect technologique ou application mobile moderne.

L'utilisateur doit avoir l'impression d'ouvrir une peinture vivante.

ARCHITECTURE

Technologies :

- HTML
- CSS
- JavaScript natif

Aucun framework.

Compatible GitHub Pages.

Responsive mobile.

EXPERIENCE UTILISATEUR

Lorsque l'utilisateur scanne le tag NFC :

- affichage immédiat de l'arbre
- aucun écran d'accueil
- aucune page intermédiaire
- aucune navigation visible

L'arbre et le paysage occupent pratiquement tout l'écran.

L'expérience doit être immersive.

ARBRE EVOLUTIF

Une constante DATE_DEBUT est définie dans le code.

Le site calcule automatiquement :

- les jours écoulés
- la progression annuelle
- le stade actuel de l'arbre

Les images sont stockées dans :

/assets/tree/

Format :

tree-001.png
tree-002.png
...
tree-052.png

Chaque image correspond à une semaine de croissance.

Après la dernière étape :

- conserver l'arbre majestueux final

SAISONS

Les arrière-plans sont stockés dans :

/assets/backgrounds/

winter.jpg
spring.jpg
summer.jpg
autumn.jpg

Le changement de saison doit être automatique selon la date.

Créer des transitions douces.

Ajouter des animations très légères :

Hiver :
- neige discrète

Printemps :
- pollen
- pétales

Été :
- lumière flottante
- lucioles

Automne :
- feuilles tombantes

Les animations doivent rester élégantes et peu visibles.

VIDEO HEBDOMADAIRE

Créer un bouton vidéo très discret.

Position :

- en haut à droite

Style :

- minimaliste
- élégant
- semi-transparent
- effet verre dépoli

Le bouton ne doit pas détourner l'attention de l'arbre.

Au clic :

- ouverture d'une fenêtre modale élégante
- lecteur vidéo intégré
- fermeture simple

La vidéo est configurée depuis :

/data/video.json

Structure :

{
  "title": "Titre",
  "video": "videos/video-semaine.mp4",
  "date": "2026-01-01"
}

Le développeur doit pouvoir remplacer la vidéo simplement en changeant le fichier.

MESSAGE D'AMBIANCE

Afficher de manière discrète :

- âge de l'arbre en jours

Ajouter un emplacement configurable pour afficher une courte phrase poétique.

Cette phrase doit être modifiable facilement.

DESIGN

Palette :

- verts naturels
- ocres
- bruns doux
- couleurs désaturées

Utiliser :

- ombres légères
- transparences
- animations lentes
- transitions fluides

Éviter :

- couleurs vives
- interfaces techniques
- boutons voyants

L'objectif est que le site ressemble davantage à une œuvre vivante qu'à une application.

LIVRABLES

Générer entièrement :

- index.html
- style.css
- script.js
- data/video.json

Inclure :

- arborescence complète du projet
- commentaires détaillés
- code prêt pour GitHub Pages

Le résultat doit être directement exploitable comme proof of concept fonctionnel.
_____________

Hello,

I am building a very personal website that is accessed through an NFC tag hidden in a piece of clothing.

The website tells a story over one year using a tree that slowly grows over time.

Before ordering the complete project, I would like to start with a single concept illustration to validate the artistic direction.

Style wanted:

- watercolor
- soft
- nostalgic
- emotional
- natural
- storybook quality

The final project may include:

- 26 to 52 tree growth stages
- 4 seasonal backgrounds
- transparent PNG assets

Could you tell me:

- if this type of project interests you
- what budget range you would recommend
- whether you can maintain visual consistency across many growth stages

Thank you.

_____
