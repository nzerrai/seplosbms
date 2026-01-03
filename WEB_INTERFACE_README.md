# 🌐 Interface Web COBOL to Java Spring Batch

## 📋 Description

Cette interface web permet de convertir facilement vos programmes COBOL en projets Spring Batch complets via une interface graphique intuitive.

## 🚀 Démarrage rapide

### 1. Lancer l'application

```bash
# Depuis le répertoire du projet
java -jar target/cobol-translator.jar

# Ou avec Maven
mvn spring-boot:run
```

### 2. Accéder à l'interface

Ouvrez votre navigateur et accédez à :
```
http://localhost:9090/conversion
```

## 📖 Utilisation

### Étape 1 : Remplir le formulaire

1. **Nom du projet** (obligatoire)
   - Exemple: `my-batch-project`
   - Sera utilisé comme nom du projet Spring Batch généré

2. **Package de base** (optionnel)
   - Exemple: `com.example.batch`
   - Par défaut: `com.example.batch`
   - Doit suivre la convention Java (minuscules, points)

3. **Fichiers COBOL**
   - Formats acceptés: `.cob`, `.cbl`
   - Vous pouvez uploader plusieurs fichiers
   - Taille maximale: 50 MB par fichier

### Étape 2 : Upload des fichiers

Vous avez deux options :

**Option A : Cliquer sur la zone d'upload**
- Cliquez sur la zone "📤 Cliquez ou glissez-déposez..."
- Sélectionnez vos fichiers COBOL

**Option B : Glisser-déposer**
- Glissez vos fichiers COBOL directement dans la zone
- Ils seront automatiquement ajoutés à la liste

### Étape 3 : Convertir

1. Vérifiez que tous les fichiers sont listés
2. Cliquez sur **"🚀 Convertir en Spring Batch"**
3. La conversion démarre automatiquement

### Étape 4 : Télécharger le projet

Une fois la conversion terminée :
- Un fichier ZIP sera automatiquement téléchargé
- Le fichier contient le projet Spring Batch complet
- Nom du fichier: `{nom-du-projet}.zip`

## 📦 Contenu du projet généré

Le projet ZIP téléchargé contient :

```
mon-projet/
├── pom.xml                          # Configuration Maven
├── README.md                        # Documentation du projet
├── src/
│   ├── main/
│   │   ├── java/
│   │   │   └── com/example/batch/
│   │   │       ├── Application.java           # Classe principale
│   │   │       ├── batch/
│   │   │       │   ├── *JobConfig.java       # Configuration des jobs
│   │   │       │   └── *Processor.java       # Processeurs de données
│   │   │       ├── model/
│   │   │       │   └── *Entity.java          # Entités de données
│   │   │       └── config/
│   │   │           └── BatchConfiguration.java
│   │   └── resources/
│   │       └── application.properties         # Configuration Spring
│   └── test/
│       └── java/
```

## 🛠️ Utiliser le projet généré

### 1. Décompresser le ZIP

```bash
unzip mon-projet.zip
cd mon-projet
```

### 2. Compiler le projet

```bash
mvn clean package
```

### 3. Exécuter le projet

```bash
# Avec Maven
mvn spring-boot:run

# Ou avec le JAR
java -jar target/*.jar
```

### 4. Accéder à la console H2 (optionnel)

Pour déboguer la base de données :
```
http://localhost:9090/h2-console

JDBC URL: jdbc:h2:mem:batchdb
Username: sa
Password: (laisser vide)
```

## ⚙️ Configuration de l'interface web

### Ports personnalisés

Port actuel configuré : **9090**

Pour changer le port, modifiez `application.properties` :

```properties
server.port=8080  # ou un autre port
```

### Taille maximale des fichiers

Configuration actuelle dans `application.properties` :

```properties
spring.servlet.multipart.max-file-size=50MB
spring.servlet.multipart.max-request-size=100MB
```

## 🎯 Exemples de conversion

### Exemple 1 : Programme COBOL simple

**Fichier d'entrée** : `customer.cob`
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-PROCESS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-ID    PIC 9(5).
       01  WS-CUSTOMER-NAME  PIC X(30).
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'Processing customers'.
           STOP RUN.
```

**Projet généré** :
- `CustomerProcessJobConfig.java` - Configuration Spring Batch
- `CustomerProcessEntity.java` - Entité avec customerId, customerName
- `CustomerProcessProcessor.java` - Processeur de données

### Exemple 2 : Plusieurs fichiers COBOL

Vous pouvez uploader plusieurs fichiers en une fois :
- `customer.cob`
- `order.cob`
- `invoice.cob`

Chaque fichier générera ses propres classes Java dans le même projet.

## 🔧 Dépannage

### Erreur : "No valid COBOL files found"

**Cause** : Les fichiers uploadés n'ont pas l'extension `.cob` ou `.cbl`

**Solution** : Renommez vos fichiers avec l'extension correcte

### Erreur : "Invalid package name"

**Cause** : Le nom du package ne respecte pas la convention Java

**Solution** : Utilisez un format valide comme `com.company.project`
- Tout en minuscules
- Séparé par des points
- Commence par une lettre

### Erreur : "Conversion failed"

**Cause** : Erreur de syntaxe dans le fichier COBOL

**Solution** :
1. Vérifiez que votre fichier COBOL compile correctement
2. Assurez-vous que les divisions sont complètes
3. Vérifiez les messages d'erreur détaillés

### L'application ne démarre pas

**Vérifiez** :
```bash
# Port déjà utilisé ?
netstat -an | grep 9090

# Java version
java -version  # Doit être Java 17+

# Relancer avec logs détaillés
java -jar target/cobol-translator.jar --debug
```

## 📊 Statistiques de conversion

L'interface affiche des informations sur :
- ✅ Nombre de fichiers uploadés
- ✅ Taille totale des fichiers
- ✅ Progression de la conversion
- ✅ Fichiers générés dans le projet

## 🔒 Sécurité

### Fichiers temporaires

- Les fichiers uploadés sont stockés temporairement
- Automatiquement supprimés après conversion
- Aucune persistance sur le serveur

### Validation

- Extension de fichier vérifiée (`.cob`, `.cbl`)
- Taille maximale limitée (50 MB)
- Validation du nom de package Java

## 💡 Conseils et bonnes pratiques

### Pour de meilleurs résultats

1. **Nommage cohérent** : Utilisez des noms de projet descriptifs
2. **Organisation** : Groupez les fichiers COBOL par fonctionnalité
3. **Validation** : Testez vos programmes COBOL avant conversion
4. **Documentation** : Le README généré contient les instructions de build

### Structure de package recommandée

```
com.{entreprise}.{domaine}.batch
```

Exemples :
- `com.company.finance.batch`
- `com.bank.customer.batch`
- `com.retail.inventory.batch`

## 🎨 Personnalisation de l'interface

### Thème et couleurs

Les fichiers CSS sont dans :
```
src/main/resources/static/css/conversion.css
```

### Textes et labels

Le template HTML est dans :
```
src/main/resources/templates/conversion.html
```

## 📝 Logs

Pour voir les logs de conversion :

```bash
# Dans le terminal où l'application tourne
# Les logs affichent :
- Fichiers en cours de parsing
- Classes générées
- Erreurs éventuelles
```

## 🚀 Déploiement en production

### Avec Docker (optionnel)

```dockerfile
FROM openjdk:17-slim
COPY target/cobol-translator.jar app.jar
EXPOSE 9090
ENTRYPOINT ["java","-jar","/app.jar"]
```

```bash
docker build -t cobol-translator .
docker run -p 9090:9090 cobol-translator
```

## 📧 Support

Pour toute question ou problème :
1. Consultez la documentation dans `/docs`
2. Vérifiez les logs de l'application
3. Testez avec des fichiers COBOL simples d'abord

## 🔄 Mises à jour

Pour mettre à jour l'application :

```bash
# Récupérer les dernières modifications
git pull

# Recompiler
mvn clean package

# Redémarrer
java -jar target/cobol-translator.jar
```

---

**Version** : 1.0.0
**Date** : 2026-01-02
**Compatibilité** : Java 17+, Spring Boot 3.2.0
