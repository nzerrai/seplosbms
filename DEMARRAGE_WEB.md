# 🚀 Démarrage de l'Interface Web COBOL to Java Translator

## Méthode recommandée: Maven Spring Boot

```bash
mvn spring-boot:run
```

L'application démarre sur **http://localhost:9090**

### Accès aux services:

- **Interface Web**: http://localhost:9090/conversion
- **Console H2**: http://localhost:9090/h2-console

## Méthode alternative: Script de démarrage

```bash
./start-web.sh
```

Ce script utilise Maven pour démarrer le serveur.

## Configuration H2 Console

Si vous accédez à la console H2:

- **JDBC URL**: `jdbc:h2:mem:translatordb`
- **Username**: `sa`
- **Password**: (laisser vide)

## Mode CLI (Ligne de commande)

Le mode CLI utilise une classe différente et ne démarre PAS le serveur web:

```bash
# Pour la conversion CLI (pas de serveur web)
java -jar target/cobol-translator.jar translate examples/mon-fichier.cob --package com.example.batch
```

## Architecture

Le projet contient deux points d'entrée:

1. **CobolTranslatorCli** - Pour les conversions en ligne de commande (classe principale du JAR)
2. **CobolTranslatorWebApplication** - Pour l'interface web (démarré via Maven)

## Ports utilisés

- **Port 9090**: Interface Web + API REST
- **Port H2**: Console de base de données (même port 9090)

## Dépannage

### Port déjà utilisé

```bash
# Trouver et tuer le processus
lsof -ti:9090 | xargs kill -9
```

### Recompiler le projet

```bash
mvn clean package
```

### Erreur de base de données

Vérifiez que la dépendance H2 est bien présente dans le pom.xml:

```xml
<dependency>
    <groupId>com.h2database</groupId>
    <artifactId>h2</artifactId>
    <scope>runtime</scope>
</dependency>
```

## Utilisation de l'interface web

1. Démarrer le serveur: `mvn spring-boot:run`
2. Ouvrir: http://localhost:9090/conversion
3. Remplir le formulaire:
   - Nom du projet (obligatoire)
   - Package de base (ex: com.example.batch)
   - Glisser-déposer les fichiers .cob/.cbl
4. Cliquer sur "Convertir en Spring Batch"
5. Le projet ZIP se télécharge automatiquement

## Logs

Pour voir les logs en détail:

```bash
mvn spring-boot:run -Dspring-boot.run.arguments=--logging.level.com.cobol.translator=DEBUG
```

---

**Version**: 1.0.0
**Date**: 2026-01-02
