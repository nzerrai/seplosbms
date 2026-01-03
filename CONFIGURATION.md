# Configuration Guide

Ce document décrit les options de configuration disponibles pour le COBOL to Java Spring Batch Translator.

## Configuration des Répertoires Temporaires

L'application utilise deux types de répertoires temporaires pendant le processus de conversion:

### 1. Répertoire d'Upload (`cobol.translator.temp.upload-dir`)

Ce répertoire stocke temporairement les fichiers COBOL et JCL uploadés par l'utilisateur pendant la conversion.

**Valeur par défaut:** `${java.io.tmpdir}/cobol-upload`

### 2. Répertoire de Sortie (`cobol.translator.temp.output-dir`)

Ce répertoire stocke temporairement les projets Spring Batch générés avant leur compression en fichier ZIP.

**Valeur par défaut:** `${java.io.tmpdir}/cobol-output`

## Configuration dans application.properties

Vous pouvez personnaliser ces répertoires dans le fichier `src/main/resources/application.properties`:

```properties
# Temporary Directories Configuration
# Directory for uploaded files during conversion
cobol.translator.temp.upload-dir=${java.io.tmpdir}/cobol-upload

# Directory for generated Spring Batch projects
cobol.translator.temp.output-dir=${java.io.tmpdir}/cobol-output
```

### Exemples de Configuration

#### Utiliser un répertoire spécifique sur Linux/Mac:
```properties
cobol.translator.temp.upload-dir=/var/tmp/cobol-translator/uploads
cobol.translator.temp.output-dir=/var/tmp/cobol-translator/output
```

#### Utiliser un répertoire spécifique sur Windows:
```properties
cobol.translator.temp.upload-dir=C:/Temp/cobol-translator/uploads
cobol.translator.temp.output-dir=C:/Temp/cobol-translator/output
```

#### Utiliser un répertoire dans le dossier utilisateur:
```properties
cobol.translator.temp.upload-dir=${user.home}/cobol-translator/uploads
cobol.translator.temp.output-dir=${user.home}/cobol-translator/output
```

## Variables d'Environnement Système Disponibles

Vous pouvez utiliser les variables Java suivantes dans la configuration:

- `${java.io.tmpdir}` - Répertoire temporaire du système
- `${user.home}` - Répertoire utilisateur
- `${user.dir}` - Répertoire de travail actuel

## Gestion Automatique des Répertoires

L'application gère automatiquement les répertoires temporaires:

1. **Création automatique**: Si les répertoires configurés n'existent pas, ils sont créés automatiquement au premier usage
2. **Noms uniques**: Chaque conversion utilise un sous-répertoire unique avec timestamp pour éviter les conflits
3. **Nettoyage**: Les fichiers temporaires sont supprimés après chaque conversion réussie

### Structure des Répertoires Temporaires

```
upload-dir/
  ├── upload-1704276000000-1234/
  │   ├── banking-transaction.cob
  │   └── banking-transaction.jcl
  └── upload-1704276100000-5678/
      └── customer.cob

output-dir/
  ├── my-batch-project-1704276000000/
  │   ├── src/
  │   ├── pom.xml
  │   └── README.md
  └── banking-project-1704276100000/
      ├── src/
      └── pom.xml
```

## Considérations pour la Production

### 1. Espace Disque

Assurez-vous que les répertoires configurés ont suffisamment d'espace disque:
- Les fichiers COBOL uploadés sont généralement petits (< 1 MB)
- Les projets générés peuvent être plus volumineux (5-20 MB selon la complexité)

### 2. Permissions

L'utilisateur qui exécute l'application doit avoir:
- Permissions de lecture/écriture sur les répertoires configurés
- Permissions de création de répertoires si les chemins n'existent pas

### 3. Sécurité

Pour un environnement de production:
- Utilisez des répertoires dédiés en dehors du système de fichiers principal
- Configurez des quotas de disque appropriés
- Mettez en place une surveillance de l'utilisation du disque
- Envisagez un nettoyage périodique des anciens répertoires temporaires

### 4. Performance

Pour optimiser les performances:
- Utilisez des SSD pour les répertoires temporaires si possible
- Évitez les partages réseau (NFS, SMB) pour les répertoires temporaires
- Sur Linux, envisagez d'utiliser `/dev/shm` pour des conversions très rapides (attention à la RAM)

## Configuration via Arguments JVM

Vous pouvez également surcharger ces propriétés via les arguments JVM au démarrage:

```bash
java -jar cobol-translator.jar \
  -Dcobol.translator.temp.upload-dir=/custom/upload/path \
  -Dcobol.translator.temp.output-dir=/custom/output/path
```

## Configuration via Variables d'Environnement

Spring Boot permet de configurer via des variables d'environnement:

```bash
export COBOL_TRANSLATOR_TEMP_UPLOAD_DIR=/custom/upload/path
export COBOL_TRANSLATOR_TEMP_OUTPUT_DIR=/custom/output/path
java -jar cobol-translator.jar
```

## Dépannage

### Problème: "Unable to create directory"

**Cause**: Permissions insuffisantes ou chemin invalide

**Solution**:
1. Vérifiez les permissions du répertoire parent
2. Vérifiez que le chemin est valide et accessible
3. Utilisez un chemin absolu plutôt que relatif

### Problème: "No space left on device"

**Cause**: Espace disque insuffisant

**Solution**:
1. Libérez de l'espace disque
2. Configurez un répertoire sur une partition avec plus d'espace
3. Mettez en place un nettoyage automatique des anciens fichiers

### Problème: Fichiers temporaires qui s'accumulent

**Cause**: Conversion échouée sans nettoyage

**Solution**:
1. Vérifiez les logs pour les erreurs de conversion
2. Nettoyez manuellement les anciens répertoires
3. Envisagez un script cron pour nettoyer les répertoires > 24h

## Exemple de Script de Nettoyage (Linux)

Créez un script `/usr/local/bin/cleanup-cobol-temp.sh`:

```bash
#!/bin/bash
# Nettoie les répertoires temporaires > 24 heures

UPLOAD_DIR="/var/tmp/cobol-translator/uploads"
OUTPUT_DIR="/var/tmp/cobol-translator/output"

# Supprime les répertoires de plus de 24 heures
find "$UPLOAD_DIR" -type d -mtime +1 -exec rm -rf {} +
find "$OUTPUT_DIR" -type d -mtime +1 -exec rm -rf {} +

echo "Cleanup completed at $(date)"
```

Ajoutez à crontab pour exécution quotidienne:
```bash
0 2 * * * /usr/local/bin/cleanup-cobol-temp.sh >> /var/log/cobol-cleanup.log 2>&1
```

## Logs de Configuration

Les logs montrent la création des répertoires:

```
2026-01-03 05:00:00 INFO  ConversionController - Created temporary base directory: /tmp/cobol-upload
2026-01-03 05:00:00 DEBUG ConversionController - Created temporary directory: /tmp/cobol-upload/upload-1704276000000-1234
2026-01-03 05:00:01 INFO  CobolConversionService - Created output base directory: /tmp/cobol-output
2026-01-03 05:00:01 INFO  CobolConversionService - Created output directory: /tmp/cobol-output/my-project-1704276000000
```

Pour activer les logs DEBUG:
```properties
logging.level.com.cobol.translator=DEBUG
```
