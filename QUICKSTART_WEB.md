# 🚀 Démarrage Rapide - Interface Web

## En 3 étapes simples

### 1️⃣ Démarrer l'application

```bash
cd /home/debian/Desktop/cobol-to-java-translator
java -jar target/cobol-translator.jar
```

Vous devriez voir :
```
  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::                (v3.2.0)

... Application running on port 9090
```

### 2️⃣ Ouvrir l'interface web

Dans votre navigateur :
```
http://localhost:9090/conversion
```

### 3️⃣ Convertir vos fichiers COBOL

1. Entrez le **nom du projet** (ex: "my-batch-project")
2. (Optionnel) Entrez le **package** (ex: "com.company.batch")
3. **Glissez-déposez** vos fichiers .cob ou .cbl
4. Cliquez sur **"🚀 Convertir en Spring Batch"**
5. Le fichier ZIP se télécharge automatiquement

## 📦 Utiliser le projet généré

```bash
# Décompresser
unzip my-batch-project.zip
cd my-batch-project

# Compiler
mvn clean package

# Exécuter
mvn spring-boot:run
```

## ✨ C'est tout !

Votre projet Spring Batch est prêt à être utilisé.

---

## 🔧 Options avancées

### Changer le port

Éditez `src/main/resources/application.properties` :
```properties
server.port=9090
```

Puis relancez :
```bash
mvn clean package
java -jar target/cobol-translator.jar
```

### Taille maximale des fichiers

Dans `application.properties` :
```properties
spring.servlet.multipart.max-file-size=100MB
spring.servlet.multipart.max-request-size=200MB
```

---

## 📚 Documentation complète

- [WEB_INTERFACE_README.md](WEB_INTERFACE_README.md) - Guide complet
- [IHM_IMPLEMENTATION_COMPLETE.md](IHM_IMPLEMENTATION_COMPLETE.md) - Détails techniques

---

**Bon développement ! 🎉**
