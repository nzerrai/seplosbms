# Test du Rapport de Conversion dans l'IHM Web

## Démarrage de l'Application

### Option 1 : Maven
```bash
cd /home/seplos/projets/cobol-to-java-translator
mvn spring-boot:run
```

### Option 2 : Script
```bash
./start-web.sh
```

L'application démarre sur **http://localhost:9090**

## Accès à l'Interface

Ouvrir un navigateur et accéder à :
```
http://localhost:9090/conversion
```

## Test de la Fonctionnalité

### Étape 1 : Préparer un Fichier COBOL

Utiliser un des exemples fournis :
```bash
examples/banking-transaction.cob
examples/filler-example.cob
```

### Étape 2 : Configuration du Projet

1. **Nom du projet** : `MyTestProject`
2. **Package de base** : `com.example.batch`
3. Options avancées (facultatif) :
   - ☑ Générer les tests
   - ☑ Générer la documentation
   - Build tool: Maven / Gradle

### Étape 3 : Upload du Fichier

Deux méthodes :
- **Glisser-déposer** : Faire glisser le fichier `.cob` sur la zone d'upload
- **Parcourir** : Cliquer sur la zone et sélectionner le fichier

### Étape 4 : Lancer la Conversion

Cliquer sur le bouton **🚀 Convertir**

Observer la progression :
1. ⏳ Parsing COBOL
2. ⏳ Construction AST
3. ⏳ Génération Java
4. ⏳ Configuration Maven

### Étape 5 : Visualiser le Rapport

Une fois la conversion terminée, le **Rapport de Conversion** s'affiche automatiquement avec :

#### 📊 Taux de Conversion
```
[████████████████████░░] 90.0%
```
- Barre verte : ≥80% (excellent)
- Barre jaune : 50-79% (moyen)
- Barre rouge : <50% (faible)

#### 🎯 Niveau de Confiance
```
🟢 HAUTE
Le code généré est de bonne qualité et nécessite une révision standard.
```

Niveaux possibles :
- 🟢 **TRÈS HAUTE** : Code fiable, prêt pour la production
- 🟢 **HAUTE** : Bonne qualité, révision standard
- 🟡 **MOYENNE** : Révision approfondie nécessaire
- 🟠 **FAIBLE** : Travail manuel important requis
- 🔴 **TRÈS FAIBLE** : Approche manuelle recommandée

#### 📈 Statistiques Détaillées

**Instructions COBOL**
| Métrique | Valeur |
|----------|--------|
| Total | 150 |
| Converties | 135 |
| Partielles | 10 |
| Non converties | 5 |

**Data Items**
| Métrique | Valeur |
|----------|--------|
| Total | 45 |
| Convertis | 43 |

### Étape 6 : Télécharger le Projet

Cliquer sur **⬇️ Télécharger le projet**

Le fichier ZIP `MyTestProject.zip` sera téléchargé automatiquement.

## Vérification du Contenu du ZIP

```bash
unzip -l MyTestProject.zip
```

Contenu attendu :
```
MyTestProject/
├── pom.xml
├── README.md
├── src/
│   ├── main/
│   │   ├── java/
│   │   │   └── com/example/batch/
│   │   │       ├── domain/
│   │   │       ├── reader/
│   │   │       ├── processor/
│   │   │       ├── writer/
│   │   │       └── config/
│   │   └── resources/
│   │       └── application.properties
│   └── test/
│       └── java/
└── docs/
    └── CONVERSION_REPORT.txt
```

## Tests Avancés

### Test avec Plusieurs Fichiers

1. Uploader plusieurs fichiers COBOL (`.cob`, `.cbl`)
2. Optionnel : Ajouter un fichier JCL (`.jcl`)
3. Le rapport affichera les statistiques agrégées

### Test des Erreurs

#### Fichier invalide
- Uploader un fichier non-COBOL
- Vérifier le message d'erreur

#### Syntaxe COBOL incorrecte
- Créer un fichier avec des erreurs de syntaxe
- Observer le message d'erreur détaillé

## Capture d'Écran Attendue

L'interface devrait afficher :

```
╔════════════════════════════════════════════════════════════╗
║                  ✅ Conversion réussie!                    ║
║                        (2s)                                ║
╠════════════════════════════════════════════════════════════╣
║                                                            ║
║  📊 Rapport de Conversion                                  ║
║  ────────────────────────────────────────                  ║
║                                                            ║
║  Taux de conversion                                        ║
║  [████████████████████░░] 90.0%                            ║
║                                                            ║
║  Niveau de confiance                                       ║
║  🟢 HAUTE                                                  ║
║  Le code généré est de bonne qualité et nécessite une     ║
║  révision standard.                                        ║
║                                                            ║
║  ┌──────────────────────────────────────────────────┐     ║
║  │ Instructions totales          150                │     ║
║  │ Converties                    135                │     ║
║  │ Partielles                    10                 │     ║
║  │ Non converties                5                  │     ║
║  └──────────────────────────────────────────────────┘     ║
║                                                            ║
║  ┌──────────────────────────────────────────────────┐     ║
║  │ Data items totaux             45                 │     ║
║  │ Data items convertis          43                 │     ║
║  └──────────────────────────────────────────────────┘     ║
║                                                            ║
║  1 fichier(s) COBOL converti(s) avec 90.0% de réussite.   ║
║                                                            ║
║  [ ⬇️ Télécharger le projet ]  [ 🔄 Nouvelle conversion ]  ║
║                                                            ║
╚════════════════════════════════════════════════════════════╝
```

## Vérifications à Effectuer

### ✅ Checklist Fonctionnelle

- [ ] L'interface web se charge correctement
- [ ] L'upload de fichier fonctionne (drag & drop et browse)
- [ ] La conversion s'exécute sans erreur
- [ ] Le rapport s'affiche avec toutes les métriques
- [ ] La barre de progression a la bonne couleur (vert/jaune/rouge)
- [ ] L'icône de confiance s'affiche correctement
- [ ] Les statistiques sont cohérentes (total = converties + partielles + non converties)
- [ ] Le bouton "Télécharger" fonctionne
- [ ] Le ZIP téléchargé contient tous les fichiers
- [ ] Le fichier CONVERSION_REPORT.txt est présent dans le ZIP

### ✅ Checklist Visuelle

- [ ] La carte du rapport a un style professionnel
- [ ] Les couleurs sont appropriées (vert pour succès, rouge pour échec)
- [ ] L'animation pulse de l'icône de confiance fonctionne
- [ ] Le layout est responsive (adapté aux petits écrans)
- [ ] Le dark mode fonctionne (si activé)

### ✅ Checklist Technique

- [ ] La réponse JSON contient tous les champs attendus
- [ ] Le ZIP est correctement encodé en Base64
- [ ] Les getters de ConversionReport fonctionnent
- [ ] Aucune erreur dans les logs serveur
- [ ] Aucune erreur dans la console navigateur (F12)

## Dépannage

### Problème : Le rapport ne s'affiche pas

**Solution** :
1. Ouvrir la console navigateur (F12)
2. Vérifier s'il y a des erreurs JavaScript
3. Vérifier que la réponse JSON contient le champ `report`

### Problème : Le téléchargement échoue

**Solution** :
1. Vérifier que `zipFileBase64` n'est pas null dans la réponse
2. Vérifier la console pour les erreurs de décodage Base64
3. Essayer avec un fichier COBOL plus petit

### Problème : Les statistiques sont incorrectes

**Solution** :
1. Vérifier les logs serveur pour voir les valeurs calculées
2. Examiner le fichier CONVERSION_REPORT.txt dans le ZIP
3. Comparer avec l'output CLI : `java -jar cobol-translator.jar input.cob`

## Logs Attendus

### Logs Serveur (Spring Boot)
```
INFO  c.c.t.c.ConversionController : Received conversion request - Project: MyTestProject, Package: com.example.batch, Files: 1
INFO  c.c.t.s.CobolConversionService : Starting conversion - Project: MyTestProject, Package: com.example.batch, COBOL Files: 1
INFO  c.c.t.s.CobolConversionService : Conversion completed successfully.
INFO  c.c.t.s.CobolConversionService : Total files generated: 12
```

### Console Navigateur (JavaScript)
```
[Conversion] Files uploaded: 1
[Conversion] Conversion started
[Progress] Parsing COBOL... (25%)
[Progress] Building AST... (50%)
[Progress] Generating Java... (75%)
[Progress] Configuring Maven... (100%)
[Conversion] Success! Displaying report
[Report] Conversion: 90.0%, Confidence: HAUTE
```

## Commandes Utiles

### Voir les logs en direct
```bash
tail -f /tmp/spring-boot-output.log
```

### Vérifier le port
```bash
netstat -tuln | grep 9090
```

### Tuer le serveur
```bash
pkill -f spring-boot:run
```

### Nettoyer les fichiers temporaires
```bash
rm -rf /tmp/cobol-*
```

## Support

En cas de problème :
1. Consulter les logs serveur
2. Consulter la documentation : `CR_CONVERSION_IHM.md`
3. Vérifier les issues Git
4. Contacter l'équipe de support

---

**Date de création** : 2026-01-07  
**Testé sur** : Linux, Chrome/Firefox  
**Version** : 1.0.0
