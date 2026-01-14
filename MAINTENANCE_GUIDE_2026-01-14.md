# 🛠️ GUIDE DE MAINTENANCE - Traducteur COBOL/JCL vers Spring Batch

**Date:** 14 Janvier 2026  
**Version:** 1.0.0-SNAPSHOT  
**Statut:** PRODUCTION READY

---

## 📦 STRUCTURE DU PROJET

```
cobol-to-java-translator/
├── src/
│   ├── main/
│   │   ├── java/com/cobol/translator/
│   │   │   ├── CobolTranslator.java          [Entry point]
│   │   │   ├── CobolTranslatorCli.java       [CLI interface]
│   │   │   ├── CobolTranslatorWebApplication.java [Web UI]
│   │   │   ├── analyzer/                     [Semantic analysis]
│   │   │   ├── ast/                          [AST nodes]
│   │   │   ├── config/                       [Configuration]
│   │   │   ├── converter/                    [Data converters]
│   │   │   ├── copybook/                     [Copybook support]
│   │   │   ├── diagram/                      [Diagram generation]
│   │   │   ├── generator/                    [Code generation]
│   │   │   ├── jcl/                          [JCL support]
│   │   │   ├── model/                        [COBOL models]
│   │   │   ├── parser/                       [COBOL parser]
│   │   │   ├── project/                      [Maven project gen]
│   │   │   ├── report/                       [Reports]
│   │   │   ├── result/                       [Results]
│   │   │   ├── semantic/                     [Semantic analysis]
│   │   │   ├── service/                      [Services]
│   │   │   ├── vsam/                         [VSAM support]
│   │   │   └── web/                          [Web components]
│   │   ├── antlr4/                           [ANTLR4 grammars]
│   │   │   ├── Cobol.g4                      [COBOL grammar]
│   │   │   └── JCL.g4                        [JCL grammar]
│   │   └── resources/
│   │       ├── translator.properties         [Configuration]
│   │       ├── application.properties        [Spring config]
│   │       └── templates/                    [Code templates]
│   └── test/
│       └── java/com/cobol/translator/        [Unit tests]
├── pom.xml                                   [Maven config]
├── target/                                   [Build output]
├── examples/                                 [Example COBOL files]
└── generated-projects/                       [Generated projects]
```

---

## 🚀 DÉMARRAGE RAPIDE

### 1. Compiler le Traducteur

```bash
cd /home/seplos/projets/cobol-to-java-translator
mvn clean install -DskipTests
```

**Temps:** ~10 secondes  
**Sortie:** `target/cobol-to-java-translator-1.0.0-SNAPSHOT.jar`

### 2. Utiliser le CLI

#### Traduire un fichier COBOL
```bash
java -cp target/classes com.cobol.translator.CobolTranslatorCli \
  translate examples/simple-customer.cob \
  -p com.mycompany.batch \
  -o src/main/java
```

#### Traduire tous les fichiers d'un répertoire
```bash
java -cp target/classes com.cobol.translator.CobolTranslatorCli \
  translate-all examples/ \
  -p com.mycompany.batch \
  -o src/main/java
```

### 3. Lancer la Web UI

```bash
java -cp target/classes com.cobol.translator.CobolTranslatorWebApplication
```

Accédez à: `http://localhost:8080`

---

## 🔧 CONFIGURATION

### translator.properties

```properties
# Package configuration
translator.target.package.base=com.generated
translator.naming.entity.suffix=Record
translator.naming.processor.suffix=Processor
translator.naming.job.suffix=Job

# Output configuration
translator.output.base.directory=../generated-projects

# VSAM configuration
translator.vsam.enabled=true
translator.vsam.jdbc.driver=org.h2.Driver
translator.vsam.jdbc.url=jdbc:h2:mem:test

# Copybook configuration
translator.copybook.search.paths=copybooks/,includes/

# Logging
translator.log.level=INFO
```

### application.properties (pour Web UI)

```properties
spring.application.name=cobol-translator
server.port=8080
server.servlet.context-path=/translator

spring.batch.job.enabled=false
spring.h2.console.enabled=true

logging.level.com.cobol.translator=DEBUG
```

---

## 🧪 TESTS

### Exécuter les Tests Unitaires

```bash
mvn test
```

### Exécuter les Tests Spécifiques

```bash
# Tests du parser
mvn test -Dtest=*ParserTest

# Tests du générateur
mvn test -Dtest=*GeneratorTest

# Tests JCL
mvn test -Dtest=JCL*Test
```

### Ajouter un Nouveau Test

```java
package com.cobol.translator.generator;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class MyGeneratorTest {
    @Test
    public void testGeneration() {
        // Arrange
        CobolProgram program = createTestProgram();
        
        // Act
        File result = generator.generate(program, config, outputDir);
        
        // Assert
        assertNotNull(result);
        assertTrue(result.exists());
    }
}
```

---

## 🐛 DÉPANNAGE COURANT

### Problème 1: "NoClassDefFoundError: picocli/CommandLine"

**Cause:** Dependencies non sur le classpath  
**Solution:**
```bash
mvn exec:java \
  -Dexec.mainClass="com.cobol.translator.CobolTranslatorCli" \
  -Dexec.args="translate ..."
```

### Problème 2: "Cannot find COBOL file"

**Cause:** Chemin relatif incorrect  
**Solution:** Utiliser chemins absolus ou vérifier répertoire de travail
```bash
cd /home/seplos/projets/cobol-to-java-translator
# Puis utiliser chemins relatifs à partir de ce répertoire
```

### Problème 3: "Copybook not found"

**Cause:** Chemin search copybooks incorrectement configuré  
**Solution:** Vérifier `translator.properties`:
```properties
translator.copybook.search.paths=/path/to/copybooks/
```

### Problème 4: "VSAM analysis failed"

**Cause:** Configuration JDBC VSAM manquante  
**Solution:** Vérifier `application.properties`:
```properties
translator.vsam.jdbc.driver=org.h2.Driver
translator.vsam.jdbc.url=jdbc:h2:mem:vsam
```

### Problème 5: Slow parsing on large files

**Cause:** ANTLR4 parsing pas optimisé  
**Solution:** 
1. Scinder fichiers COBOL grands
2. Activer caching AST (future optimization)
3. Utiliser mode batch processing

---

## 📊 MONITORING EN PRODUCTION

### Logs à Surveiller

```bash
# Tail logs
tail -f app.log | grep "ERROR\|WARN"

# Chiffres clés
grep "Translation completed" app.log | wc -l  # Succès
grep "Translation failed" app.log | wc -l     # Échecs
```

### Métriques Clés

| Métrique | Alerte Si |
|----------|-----------|
| Temps traduction | > 5 secondes |
| Taille fichier généré | > 10 MB |
| Erreurs parsing | > 0 |
| Warnings conversion | > 5% |
| Succès traduction | < 100% |

### Health Check

```bash
curl http://localhost:8080/translator/actuator/health
```

---

## 🔄 PIPELINE DE DÉPLOIEMENT

### Phase 1: Build
```bash
mvn clean package -DskipTests
```

### Phase 2: Test
```bash
mvn test
mvn integration-test
```

### Phase 3: Package JAR
```bash
mvn assembly:single
```

### Phase 4: Déployer
```bash
java -jar target/cobol-to-java-translator-1.0.0-SNAPSHOT.jar
```

### Phase 5: Vérifier
```bash
curl http://localhost:8080/translator
```

---

## 📈 AMÉLIORATIONS FUTURES

### Sprint 1 (Priorité Haute)
- [ ] Implémenter TestGenerator complètement
- [ ] Ajouter performance benchmarks
- [ ] Optimiser AST caching
- [ ] Support copybooks conditionnels

### Sprint 2 (Priorité Moyenne)
- [ ] Web UI enhancements
- [ ] Advanced type inference ML
- [ ] CICS transaction support
- [ ] DB2 SQL generation

### Sprint 3+ (Backlog)
- [ ] Cloud-native support (K8s)
- [ ] Monitoring/tracing distribué
- [ ] Support langages additionnels
- [ ] AI-assisted refactoring

---

## 📚 RESSOURCES UTILES

### Documentation Interne
- [AUDIT_TRANSLATOR_2026-01-14.md](AUDIT_TRANSLATOR_2026-01-14.md) - Audit complet
- [TEST_EXECUTION_REPORT_2026-01-14.md](TEST_EXECUTION_REPORT_2026-01-14.md) - Résultats tests
- [PHASE1_SUMMARY.md](PHASE1_SUMMARY.md) - Architecture Phase 1
- [JCL_SUPPORT.md](JCL_SUPPORT.md) - Support JCL détail

### Documentation Externe
- [ANTLR4 Documentation](https://www.antlr.org/wiki/display/ANTLR4/Home)
- [Spring Batch Reference](https://spring.io/projects/spring-batch)
- [Spring Boot 3.2 Guide](https://spring.io/projects/spring-boot)

---

## 👥 CONTACTS & SUPPORT

### En cas de Problème
1. Consulter ce guide de maintenance
2. Vérifier les logs: `tail -f app.log`
3. Exécuter les tests: `mvn test`
4. Consulter les rapports d'audit
5. Kontacter l'équipe développement

### Équipe Responsable
- **Lead Developer:** MIGRATION-TEAM
- **Maintainer:** @cobol-translator-team
- **Support:** documentation/issues dans le repo

---

## 🔐 SÉCURITÉ

### Points de Sécurité Importants

1. **Input Validation:**
   - Vérifier fichiers COBOL avant traduction
   - Valider chemins fichiers
   - Sanitizer noms packages

2. **Dépendances:**
   - Mise à jour régulière Spring Boot
   - Audit CVE periodique
   - Freeze versions stables

3. **Données Sensibles:**
   - Pas de mots de passe en properties
   - Utiliser variables d'environnement
   - Chiffrer config si nécessaire

### Checklist Déploiement

- [ ] Dépendances à jour
- [ ] Logs configurés
- [ ] Monitoring actif
- [ ] Backups en place
- [ ] Tests passent 100%
- [ ] Documentation à jour
- [ ] Security scan complet

---

## 📋 CHANGELOG RÉCENT

### v1.0.0 (2026-01-14) - RELEASE CANDIDATE
- ✅ Architecture complète ANTLR4 + AST
- ✅ Support COBOL complet (IDENTIFICATION/ENVIRONMENT/DATA/PROCEDURE)
- ✅ Support JCL parsing + generation
- ✅ Spring Batch generation avec Entity/Processor/Configuration
- ✅ Copybook resolution
- ✅ VSAM file analysis + JDBC mapping
- ✅ Algorithm diagrams generation
- ✅ Conversion reports avec type mapping
- ⚠️ TestGenerator skeleton (à compléter)
- ⚠️ Performance optimization (future)

---

## 📞 SUPPORT OPÉRATIONNEL

### Runbook Production

#### Démarrer le Service
```bash
systemctl start cobol-translator
# ou
java -jar /opt/cobol-translator/cobol-to-java-translator-1.0.0.jar
```

#### Arrêter le Service
```bash
systemctl stop cobol-translator
# ou
kill -SIGTERM $PID
```

#### Vérifier Status
```bash
systemctl status cobol-translator
curl http://localhost:8080/translator/actuator/health
```

#### Recharger Configuration
```bash
systemctl reload cobol-translator
```

#### Consulter Logs
```bash
journalctl -u cobol-translator -f
# ou
tail -f /var/log/cobol-translator/translator.log
```

---

**Dernier update:** 2026-01-14  
**Prochaine review:** 2026-02-14
