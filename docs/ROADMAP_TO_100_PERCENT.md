# 🎯 ROADMAP VERS 100% DE CONVERSION COBOL → JAVA

**Document de Planification Stratégique**
**Version:** 1.0
**Date:** 08 janvier 2026
**Objectif:** Atteindre 99%+ de taux de conversion automatique

---

## 📊 ÉTAT DES LIEUX

### Métriques Actuelles

| Indicateur | Valeur Actuelle | Cible |
|------------|----------------|-------|
| **Taux de conversion moyen** | 76-82% | 99%+ |
| **Constructions COBOL supportées** | 82% (80/98) | 99% (97/98) |
| **TODOs non résolus** | 105 | 0 |
| **Couverture de tests** | ~80% | 95%+ |
| **Support EXEC SQL** | 0% | 95% |
| **Support EXEC CICS** | 0% | 90% |
| **REDEFINES complexes** | 70% | 95% |

### Gaps Principaux Identifiés

1. **EXEC SQL/CICS** non supportés (impact: -15 à -20%)
2. **105 TODOs** non implémentés (impact: -5 à -8%)
3. **REDEFINES complexes** incomplets (impact: -3 à -5%)
4. **Tests E2E** absents (impact qualité)
5. **OCCURS DEPENDING ON** partiellement supporté (impact: -2 à -3%)

---

## 🎯 PLAN D'ACTION GLOBAL

### Vue d'Ensemble des Phases

```
┌────────────────────────────────────────────────────────────────┐
│ PHASE 1: FONDATIONS CRITIQUES (3 mois)                        │
│ Objectif: 82% → 92-95%                                         │
│ • Support EXEC SQL → JPA                                       │
│ • Support EXEC CICS → REST API                                 │
│ • Résolution de tous les TODOs                                 │
│ • REDEFINES complexes                                          │
└────────────────────────────────────────────────────────────────┘
                              ↓
┌────────────────────────────────────────────────────────────────┐
│ PHASE 2: ROBUSTESSE & QUALITÉ (2 mois)                        │
│ Objectif: 92-95% → 97-98%                                      │
│ • OCCURS DEPENDING ON dynamique                                │
│ • EVALUATE ALSO avancé                                         │
│ • INSPECT combiné (TALLYING + REPLACING)                       │
│ • SORT gros volumes (external sort)                            │
│ • Suite de tests complète (E2E, performance)                   │
└────────────────────────────────────────────────────────────────┘
                              ↓
┌────────────────────────────────────────────────────────────────┐
│ PHASE 3: EXCELLENCE & OPTIMISATION (1.5 mois)                 │
│ Objectif: 97-98% → 99-100%                                     │
│ • Refactorisation automatique GO TO                            │
│ • Génération améliorée (validation JPA, relations, etc.)       │
│ • Support CI/CD (GitHub Actions, GitLab CI, Jenkins)           │
│ • UI Web améliorée (drag & drop, temps réel, comparaison)     │
└────────────────────────────────────────────────────────────────┘
```

### Effort Total Estimé

| Phase | Effort (jours-homme) | Durée (avec 2-3 devs) | Gain Conversion |
|-------|---------------------|----------------------|-----------------|
| **Phase 1** | 130 jours | 3 mois | +10 à +13% |
| **Phase 2** | 61 jours | 2 mois | +5 à +6% |
| **Phase 3** | 65 jours | 1.5 mois | +2 à +3% |
| **TOTAL** | **256 jours** | **6.5 mois** | **+17 à +22%** |

**Note:** Avec équipe de 3 développeurs seniors: **~8-9 mois**

---

# 🔴 PHASE 1: FONDATIONS CRITIQUES

**Durée:** 3 mois
**Équipe:** 2-3 développeurs
**Objectif:** Passer de 82% à 92-95% de conversion

---

## EPIC 1.1: Support EXEC SQL → Spring Data JPA

**Priorité:** 🔴 CRITIQUE
**Impact:** +10 à +12% conversion
**Effort:** 40 jours
**Complexité:** Élevée

### Contexte

30-40% des programmes COBOL mainframe utilisent EXEC SQL pour accéder aux bases DB2/Oracle/IMS. Cette fonctionnalité est **critique** pour la migration d'applications d'entreprise.

### Objectifs

- Parser les statements EXEC SQL embarqués dans COBOL
- Extraire les requêtes SQL et les mapper vers Spring Data JPA
- Générer des repositories JPA avec méthodes typées
- Gérer les curseurs, transactions, et gestion d'erreurs

### User Stories

#### US-1.1.1: Parser EXEC SQL dans COBOL

**En tant que** développeur du convertisseur
**Je veux** parser les blocs EXEC SQL dans les programmes COBOL
**Afin de** extraire les requêtes SQL et leurs paramètres host variables

**Critères d'acceptation:**
- [ ] Extension de la grammaire ANTLR pour EXEC SQL
- [ ] Détection des blocs EXEC SQL ... END-EXEC
- [ ] Extraction des host variables (variables COBOL utilisées en SQL)
- [ ] Support des statements: SELECT, INSERT, UPDATE, DELETE
- [ ] Gestion des curseurs (DECLARE CURSOR, OPEN, FETCH, CLOSE)
- [ ] Gestion des transactions (COMMIT, ROLLBACK)

**Tests:**
```cobol
EXEC SQL
  SELECT CUST_ID, CUST_NAME, CUST_BALANCE
  INTO :WS-CUST-ID, :WS-CUST-NAME, :WS-CUST-BALANCE
  FROM CUSTOMER
  WHERE CUST_STATUS = :WS-STATUS
END-EXEC.
```

**Tâches techniques:**
1. Créer `ExecSqlLexer.g4` et `ExecSqlParser.g4` (4 jours)
2. Implémenter `ExecSqlASTBuilder` (3 jours)
3. Créer modèle AST pour statements SQL (2 jours)
4. Tests unitaires (parser SQL simple, complexe, curseurs) (3 jours)

**Estimation:** 12 jours

---

#### US-1.1.2: Mapper EXEC SQL SELECT vers JPA Repository

**En tant que** développeur du convertisseur
**Je veux** convertir les SELECT EXEC SQL en méthodes de repository JPA
**Afin de** générer du code Java idiomatique et type-safe

**Critères d'acceptation:**
- [ ] SELECT simple → findBy...()
- [ ] SELECT avec WHERE → méthode query dynamique
- [ ] SELECT avec JOIN → @Query avec JPQL
- [ ] SELECT INTO host variables → mapping vers entité
- [ ] Gestion des résultats multiples (curseurs) → List<Entity>
- [ ] Gestion des résultats uniques → Optional<Entity>

**Exemple:**

```cobol
EXEC SQL
  SELECT CUST_ID, CUST_NAME
  INTO :WS-CUST-ID, :WS-CUST-NAME
  FROM CUSTOMER
  WHERE CUST_STATUS = :WS-STATUS
END-EXEC.
```

→

```java
@Repository
public interface CustomerRepository extends JpaRepository<Customer, String> {

    @Query("SELECT c FROM Customer c WHERE c.status = :status")
    Optional<Customer> findByStatus(@Param("status") String status);
}

// Dans le processor:
Optional<Customer> customer = customerRepository.findByStatus(record.getWsStatus());
if (customer.isPresent()) {
    record.setWsCustId(customer.get().getCustId());
    record.setWsCustName(customer.get().getCustName());
}
```

**Tâches techniques:**
1. Créer `ExecSqlToJpaTranslator` (5 jours)
2. Créer `RepositoryMethodGenerator` (4 jours)
3. Mapper types SQL → types Java/JPA (2 jours)
4. Gérer les jointures (FROM table1, table2) (3 jours)
5. Tests unitaires (SELECT simple, WHERE, JOIN) (4 jours)

**Estimation:** 18 jours

---

#### US-1.1.3: Mapper EXEC SQL INSERT/UPDATE/DELETE vers JPA

**En tant que** développeur du convertisseur
**Je veux** convertir INSERT/UPDATE/DELETE en méthodes JPA
**Afin de** gérer les opérations d'écriture en base

**Critères d'acceptation:**
- [ ] INSERT → repository.save()
- [ ] UPDATE → repository.save() avec entité existante
- [ ] DELETE → repository.deleteById() ou delete()
- [ ] Gestion des erreurs SQL (SQLCODE) → exceptions Java
- [ ] Support des transactions

**Exemple:**

```cobol
EXEC SQL
  INSERT INTO CUSTOMER (CUST_ID, CUST_NAME, CUST_STATUS)
  VALUES (:WS-CUST-ID, :WS-CUST-NAME, :WS-STATUS)
END-EXEC.

IF SQLCODE NOT = 0
   MOVE 'ERROR' TO WS-ERROR-FLAG
END-IF.
```

→

```java
try {
    Customer newCustomer = new Customer();
    newCustomer.setCustId(record.getWsCustId());
    newCustomer.setCustName(record.getWsCustName());
    newCustomer.setStatus(record.getWsStatus());

    customerRepository.save(newCustomer);

} catch (DataAccessException e) {
    record.setWsErrorFlag("ERROR");
    logger.error("Failed to insert customer: {}", e.getMessage());
}
```

**Tâches techniques:**
1. Implémenter traduction INSERT (3 jours)
2. Implémenter traduction UPDATE (3 jours)
3. Implémenter traduction DELETE (2 jours)
4. Gestion des erreurs SQLCODE → exceptions (2 jours)
5. Tests unitaires (CRUD complet) (3 jours)

**Estimation:** 13 jours

---

#### US-1.1.4: Gérer les curseurs EXEC SQL

**En tant que** développeur du convertisseur
**Je veux** convertir les curseurs SQL en streams/itérateurs Java
**Afin de** gérer les ensembles de résultats volumineux

**Critères d'acceptation:**
- [ ] DECLARE CURSOR → définition de query
- [ ] OPEN CURSOR → exécution de la query
- [ ] FETCH CURSOR → itération sur les résultats
- [ ] CLOSE CURSOR → libération des ressources
- [ ] Support curseurs avec paramètres

**Exemple:**

```cobol
EXEC SQL
  DECLARE C1 CURSOR FOR
  SELECT CUST_ID, CUST_NAME
  FROM CUSTOMER
  WHERE CUST_STATUS = :WS-STATUS
END-EXEC.

EXEC SQL OPEN C1 END-EXEC.

PERFORM UNTIL SQLCODE NOT = 0
   EXEC SQL
      FETCH C1 INTO :WS-CUST-ID, :WS-CUST-NAME
   END-EXEC

   IF SQLCODE = 0
      PERFORM PROCESS-CUSTOMER
   END-IF
END-PERFORM.

EXEC SQL CLOSE C1 END-EXEC.
```

→

```java
// Utilisation de Stream pour curseur
try (Stream<Customer> customerStream = customerRepository.findByStatusStream(record.getWsStatus())) {
    customerStream.forEach(customer -> {
        record.setWsCustId(customer.getCustId());
        record.setWsCustName(customer.getCustName());
        processCustomer(record);
    });
}
```

**Tâches techniques:**
1. Détecter et parser DECLARE/OPEN/FETCH/CLOSE (3 jours)
2. Générer Stream API pour curseurs (4 jours)
3. Gérer fin de curseur (SQLCODE = 100) (2 jours)
4. Tests unitaires (curseurs simples, avec paramètres) (3 jours)

**Estimation:** 12 jours

---

#### Résumé EPIC 1.1

**Stories:** 4
**Effort total:** 55 jours (optimisé à 40 jours avec parallélisation)
**Tests:** 13 jours inclus

---

## EPIC 1.2: Support EXEC CICS → REST API Spring MVC

**Priorité:** 🔴 CRITIQUE
**Impact:** +8 à +10% conversion
**Effort:** 40 jours
**Complexité:** Très élevée

### Contexte

CICS (Customer Information Control System) est le moniteur transactionnel mainframe standard. Les programmes COBOL CICS gèrent les transactions online (écrans 3270, API).

**Changement de paradigme:**
- COBOL CICS: Application terminal-based synchrone
- Java: Application REST API asynchrone (microservices)

### Objectifs

- Parser les commandes EXEC CICS
- Mapper CICS READ/WRITE vers REST GET/POST/PUT/DELETE
- Générer des Controllers Spring MVC
- Gérer les transactions et sessions

### User Stories

#### US-1.2.1: Parser EXEC CICS dans COBOL

**En tant que** développeur du convertisseur
**Je veux** parser les blocs EXEC CICS dans les programmes COBOL
**Afin de** extraire les opérations transactionnelles

**Critères d'acceptation:**
- [ ] Extension grammaire ANTLR pour EXEC CICS
- [ ] Support commandes: READ, WRITE, REWRITE, DELETE, SEND, RECEIVE
- [ ] Support gestion d'erreurs: RESP, HANDLE CONDITION
- [ ] Support transactions: SYNCPOINT, SYNCPOINT ROLLBACK
- [ ] Extraction des noms de fichiers/queues

**Tests:**
```cobol
EXEC CICS READ
  FILE('CUSTFILE')
  INTO(WS-CUSTOMER-RECORD)
  RIDFLD(WS-CUST-ID)
  RESP(WS-RESP-CODE)
END-EXEC.
```

**Tâches techniques:**
1. Créer grammaire ANTLR EXEC CICS (5 jours)
2. Implémenter `ExecCicsASTBuilder` (4 jours)
3. Modèle AST pour commandes CICS (3 jours)
4. Tests unitaires (READ, WRITE, SEND, etc.) (4 jours)

**Estimation:** 16 jours

---

#### US-1.2.2: Mapper EXEC CICS FILE vers REST API

**En tant que** développeur du convertisseur
**Je veux** convertir les opérations CICS FILE en endpoints REST
**Afin de** remplacer les accès fichiers VSAM par des API RESTful

**Critères d'acceptation:**
- [ ] CICS READ → GET /api/{resource}/{id}
- [ ] CICS WRITE → POST /api/{resource}
- [ ] CICS REWRITE → PUT /api/{resource}/{id}
- [ ] CICS DELETE → DELETE /api/{resource}/{id}
- [ ] Gestion des codes retour RESP → HTTP status codes
- [ ] Génération de DTOs à partir des records COBOL

**Exemple:**

```cobol
EXEC CICS READ
  FILE('CUSTFILE')
  INTO(WS-CUSTOMER-RECORD)
  RIDFLD(WS-CUST-ID)
  RESP(WS-RESP-CODE)
END-EXEC.

IF WS-RESP-CODE = DFHRESP(NORMAL)
   MOVE 'OK' TO WS-STATUS
ELSE
   MOVE 'ERROR' TO WS-STATUS
END-IF.
```

→

```java
@RestController
@RequestMapping("/api/customers")
public class CustomerController {

    @Autowired
    private CustomerService customerService;

    @GetMapping("/{id}")
    public ResponseEntity<CustomerDto> getCustomer(@PathVariable String id) {
        try {
            CustomerDto customer = customerService.findById(id);
            return ResponseEntity.ok(customer);
        } catch (EntityNotFoundException e) {
            return ResponseEntity.notFound().build();
        }
    }

    @PostMapping
    public ResponseEntity<CustomerDto> createCustomer(@RequestBody CustomerDto customer) {
        CustomerDto created = customerService.create(customer);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @PutMapping("/{id}")
    public ResponseEntity<CustomerDto> updateCustomer(
            @PathVariable String id,
            @RequestBody CustomerDto customer) {
        CustomerDto updated = customerService.update(id, customer);
        return ResponseEntity.ok(updated);
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteCustomer(@PathVariable String id) {
        customerService.delete(id);
        return ResponseEntity.noContent().build();
    }
}
```

**Tâches techniques:**
1. Créer `CicsToRestTranslator` (5 jours)
2. Créer `RestControllerGenerator` (5 jours)
3. Créer `DtoGenerator` à partir de records COBOL (3 jours)
4. Mapper codes RESP CICS → HTTP status (2 jours)
5. Tests unitaires (CRUD complet) (4 jours)

**Estimation:** 19 jours

---

#### US-1.2.3: Mapper EXEC CICS SEND/RECEIVE vers API REST

**En tant que** développeur du convertisseur
**Je veux** convertir les écrans CICS (SEND MAP, RECEIVE MAP) en API REST
**Afin de** remplacer les terminaux 3270 par des interfaces web/mobile

**Critères d'acceptation:**
- [ ] SEND MAP → Response JSON
- [ ] RECEIVE MAP → Request JSON
- [ ] Mapping des champs BMS (Basic Mapping Support) → DTOs
- [ ] Génération de documentation OpenAPI/Swagger

**Exemple:**

```cobol
EXEC CICS SEND MAP('CUSTMAP')
  MAPSET('CUSTSET')
  FROM(WS-CUSTOMER-SCREEN)
  ERASE
END-EXEC.

EXEC CICS RECEIVE MAP('CUSTMAP')
  MAPSET('CUSTSET')
  INTO(WS-CUSTOMER-SCREEN)
END-EXEC.
```

→

```java
// Endpoint pour envoyer les données
@GetMapping("/screen/customer")
public CustomerScreenDto getCustomerScreen(@RequestParam String custId) {
    Customer customer = customerService.findById(custId);
    return mapToScreenDto(customer);
}

// Endpoint pour recevoir les données
@PostMapping("/screen/customer")
public ResponseEntity<Void> submitCustomerScreen(
        @RequestBody CustomerScreenDto screenData) {
    customerService.processScreenInput(screenData);
    return ResponseEntity.ok().build();
}

// DTO généré à partir du BMS
public class CustomerScreenDto {
    private String custId;
    private String custName;
    private String custAddress;
    // ... autres champs de l'écran
}
```

**Tâches techniques:**
1. Parser définitions BMS (mapsets) (4 jours)
2. Générer DTOs à partir des maps (3 jours)
3. Créer endpoints SEND/RECEIVE (3 jours)
4. Générer documentation Swagger (2 jours)
5. Tests unitaires (3 jours)

**Estimation:** 15 jours

---

#### US-1.2.4: Gérer les transactions CICS

**En tant que** développeur du convertisseur
**Je veux** convertir les transactions CICS en transactions Spring
**Afin de** garantir la cohérence des données

**Critères d'acceptation:**
- [ ] SYNCPOINT → @Transactional commit
- [ ] SYNCPOINT ROLLBACK → @Transactional rollback
- [ ] Gestion des unités de travail (UOW)
- [ ] Configuration isolation levels

**Exemple:**

```cobol
EXEC CICS WRITE
  FILE('CUSTFILE')
  FROM(WS-CUSTOMER-RECORD)
  RIDFLD(WS-CUST-ID)
END-EXEC.

IF SQLCODE = 0
   EXEC CICS SYNCPOINT END-EXEC
ELSE
   EXEC CICS SYNCPOINT ROLLBACK END-EXEC
END-IF.
```

→

```java
@Service
public class CustomerService {

    @Transactional
    public void createCustomer(CustomerDto customerDto) {
        try {
            Customer customer = mapToEntity(customerDto);
            customerRepository.save(customer);
            // Commit automatique si pas d'exception
        } catch (Exception e) {
            // Rollback automatique sur exception
            throw new BusinessException("Failed to create customer", e);
        }
    }
}
```

**Tâches techniques:**
1. Détecter SYNCPOINT/ROLLBACK (2 jours)
2. Générer annotations @Transactional (2 jours)
3. Configurer isolation levels (2 jours)
4. Tests unitaires (transactions, rollback) (3 jours)

**Estimation:** 9 jours

---

#### Résumé EPIC 1.2

**Stories:** 4
**Effort total:** 59 jours (optimisé à 40 jours avec parallélisation)
**Tests:** 14 jours inclus

---

## EPIC 1.3: Résolution de Tous les TODOs

**Priorité:** 🔴 CRITIQUE
**Impact:** +5 à +8% conversion
**Effort:** 30 jours
**Complexité:** Moyenne

### Contexte

Le code contient actuellement **105 TODOs** qui représentent des fonctionnalités non implémentées ou des cas limites non gérés.

### Répartition des TODOs

| Composant | TODOs | Priorité |
|-----------|-------|----------|
| BusinessLogicTranslator | 33 | 🔴 Haute |
| JobConfigGenerator | 4 | 🟡 Moyenne |
| JCLSpringBatchGenerator | 2 | 🟡 Moyenne |
| BusinessRuleGenerator | 3 | 🟢 Basse |
| Autres | 63 | Variable |

### User Stories

#### US-1.3.1: Compléter BusinessLogicTranslator

**En tant que** développeur du convertisseur
**Je veux** résoudre les 33 TODOs dans BusinessLogicTranslator
**Afin de** générer du code Java complet et fonctionnel

**Critères d'acceptation:**
- [ ] Tous les statements invalides génèrent du code valide ou lèvent une exception explicite
- [ ] Parser TALLYING clause complètement implémenté
- [ ] Parser REPLACING clause complètement implémenté
- [ ] Gestion des cas limites (null, empty, invalid)
- [ ] Actions WHEN complètes pour SEARCH/EVALUATE
- [ ] Validation de tous les statements avant génération

**TODOs à résoudre:**

1. **Validation des statements**
```java
// TODO: Invalid EVALUATE statement
// TODO: Invalid MOVE - missing source or target
// TODO: Invalid COMPUTE - missing target or expression
// TODO: PERFORM statement without paragraph name
```

**Solution:**
```java
private String validateStatement(Statement stmt, String... requiredFields) {
    if (stmt == null) {
        throw new TranslationException("Statement cannot be null");
    }

    for (String field : requiredFields) {
        if (field == null || field.trim().isEmpty()) {
            throw new TranslationException(
                String.format("Required field missing in %s statement", stmt.getType())
            );
        }
    }
    return null; // Valid
}
```

2. **Parser INSPECT TALLYING/REPLACING**
```java
// TODO: Parse TALLYING clause properly
// TODO: Parse REPLACING clause properly
```

**Solution:** Créer parser dédié avec regex ou ANTLR pour extraire:
- Mode (ALL, LEADING, FIRST)
- Caractère à compter/remplacer
- Variable compteur

3. **Actions SEARCH/EVALUATE**
```java
// TODO: Add action when found
// TODO: Add action when NOT found
// TODO: Add WHEN condition
```

**Solution:** Générer blocs if/else avec actions par défaut ou placeholder commenté

**Tâches techniques:**
1. Audit complet des 33 TODOs (2 jours)
2. Implémenter validation stricte statements (3 jours)
3. Parser INSPECT avancé (3 jours)
4. Compléter actions SEARCH/EVALUATE (2 jours)
5. Gestion erreurs et exceptions (2 jours)
6. Tests unitaires pour chaque TODO résolu (5 jours)
7. Tests d'intégration (3 jours)

**Estimation:** 20 jours

---

#### US-1.3.2: Compléter les autres générateurs

**En tant que** développeur du convertisseur
**Je veux** résoudre les TODOs dans JobConfigGenerator, JCLSpringBatchGenerator, BusinessRuleGenerator
**Afin de** générer des projets Spring Batch complets

**Critères d'acceptation:**
- [ ] JobConfigGenerator implémente la logique métier des paragraphes
- [ ] JCLSpringBatchGenerator génère tous les Step beans
- [ ] BusinessRuleGenerator ajoute les validations métier

**TODOs à résoudre:**

1. **JobConfigGenerator**
```java
// TODO: Implement logic from COBOL paragraph
// TODO: Format audit trail record
// TODO: Format error report record
```

**Solution:** Utiliser BusinessLogicTranslator pour générer la logique

2. **JCLSpringBatchGenerator**
```java
// TODO: Implement Step beans
// TODO: Implement business logic from COBOL program
```

**Solution:** Générer Step complets avec reader/processor/writer

3. **BusinessRuleGenerator**
```java
// TODO: Add account status validation
// TODO: Implement validation methods based on COBOL logic
```

**Solution:** Extraire règles métier du COBOL et générer validators

**Tâches techniques:**
1. Audit TODOs JobConfigGenerator (1 jour)
2. Implémenter génération logique paragraphes (3 jours)
3. Audit TODOs JCLSpringBatchGenerator (1 jour)
4. Implémenter génération Step complets (3 jours)
5. Audit TODOs BusinessRuleGenerator (1 jour)
6. Implémenter génération validators (2 jours)
7. Tests unitaires (4 jours)

**Estimation:** 15 jours

---

#### Résumé EPIC 1.3

**Stories:** 2
**Effort total:** 35 jours (optimisé à 30 jours)
**Tests:** 9 jours inclus

---

## EPIC 1.4: Support REDEFINES Complexes

**Priorité:** 🔴 CRITIQUE
**Impact:** +3 à +5% conversion
**Effort:** 20 jours
**Complexité:** Élevée

### Contexte

REDEFINES en COBOL permet de créer des vues multiples sur la même zone mémoire (union types). Le support actuel ne gère que la première redéfinition.

### Objectifs

- Détecter toutes les redéfinitions (multiples) d'un même champ
- Générer des classes wrapper avec vues multiples
- Optimiser l'accès mémoire

### User Stories

#### US-1.4.1: Analyser les redéfinitions multiples

**En tant que** développeur du convertisseur
**Je veux** détecter toutes les clauses REDEFINES sur un même champ
**Afin de** générer du code Java gérant toutes les vues

**Critères d'acceptation:**
- [ ] Détection de redéfinitions en chaîne
- [ ] Détection de redéfinitions hiérarchiques
- [ ] Analyse de compatibilité des types
- [ ] Rapport d'incompatibilités

**Exemple:**
```cobol
01 WS-DATA.
   05 WS-FIELD-1   PIC X(10).
   05 WS-FIELD-2 REDEFINES WS-FIELD-1 PIC 9(10).
   05 WS-FIELD-3 REDEFINES WS-FIELD-1.
      10 WS-SUB-1  PIC X(5).
      10 WS-SUB-2  PIC X(5).
```

**Analyse attendue:**
```
WS-FIELD-1 a 2 redéfinitions:
  - WS-FIELD-2 (numeric, 10 digits)
  - WS-FIELD-3 (structured, 2 sub-fields)
```

**Tâches techniques:**
1. Créer `RedefinesAnalyzer` (3 jours)
2. Implémenter détection chaînée (2 jours)
3. Analyser compatibilité types (2 jours)
4. Tests unitaires (2 jours)

**Estimation:** 9 jours

---

#### US-1.4.2: Générer classes wrapper pour unions

**En tant que** développeur du convertisseur
**Je veux** générer des classes Java avec vues multiples sur les données
**Afin de** émuler le comportement REDEFINES

**Critères d'acceptation:**
- [ ] Classe wrapper avec stockage byte[] brut
- [ ] Méthodes getter/setter pour chaque vue
- [ ] Conversion automatique entre types
- [ ] Documentation des vues disponibles

**Exemple généré:**

```java
/**
 * Wrapper for COBOL REDEFINES: WS-FIELD-1
 *
 * Available views:
 * - asString(): View as WS-FIELD-1 (PIC X(10))
 * - asNumeric(): View as WS-FIELD-2 (PIC 9(10))
 * - asStructured(): View as WS-FIELD-3 (structured)
 */
public class WsDataWrapper {

    // Stockage brut (10 bytes)
    private byte[] rawData = new byte[10];

    // Vue 1: String (WS-FIELD-1)
    public String asString() {
        return new String(rawData, StandardCharsets.UTF_8).trim();
    }

    public void setAsString(String value) {
        byte[] bytes = value.getBytes(StandardCharsets.UTF_8);
        System.arraycopy(bytes, 0, rawData, 0, Math.min(bytes.length, 10));
    }

    // Vue 2: Numeric (WS-FIELD-2)
    public BigDecimal asNumeric() {
        String numStr = new String(rawData, StandardCharsets.UTF_8).trim();
        return new BigDecimal(numStr);
    }

    public void setAsNumeric(BigDecimal value) {
        String numStr = String.format("%010d", value.longValue());
        setAsString(numStr);
    }

    // Vue 3: Structured (WS-FIELD-3)
    public WsField3Structured asStructured() {
        String sub1 = new String(rawData, 0, 5, StandardCharsets.UTF_8).trim();
        String sub2 = new String(rawData, 5, 5, StandardCharsets.UTF_8).trim();
        return new WsField3Structured(sub1, sub2);
    }

    public void setAsStructured(WsField3Structured value) {
        byte[] bytes1 = value.getWsSub1().getBytes(StandardCharsets.UTF_8);
        byte[] bytes2 = value.getWsSub2().getBytes(StandardCharsets.UTF_8);
        System.arraycopy(bytes1, 0, rawData, 0, Math.min(bytes1.length, 5));
        System.arraycopy(bytes2, 0, rawData, 5, Math.min(bytes2.length, 5));
    }
}

// Classe pour vue structurée
public class WsField3Structured {
    private String wsSub1;
    private String wsSub2;

    // Constructor, getters, setters
}
```

**Tâches techniques:**
1. Créer `UnionTypeGenerator` (4 jours)
2. Implémenter génération vues multiples (3 jours)
3. Gérer conversions de types (2 jours)
4. Tests unitaires (3 jours)

**Estimation:** 12 jours

---

#### US-1.4.3: Optimiser accès mémoire

**En tant que** développeur du convertisseur
**Je veux** optimiser les conversions entre vues
**Afin de** minimiser l'impact performance

**Critères d'acceptation:**
- [ ] Cache des conversions coûteuses
- [ ] Lazy loading des vues
- [ ] Détection des vues inutilisées

**Tâches techniques:**
1. Créer `RedefinesOptimizer` (2 jours)
2. Implémenter cache conversions (1 jour)
3. Tests performance (2 jours)

**Estimation:** 5 jours

---

#### Résumé EPIC 1.4

**Stories:** 3
**Effort total:** 26 jours (optimisé à 20 jours)
**Tests:** 7 jours inclus

---

## Résumé Phase 1

| EPIC | Effort | Impact |
|------|--------|--------|
| 1.1 - EXEC SQL | 40 jours | +10-12% |
| 1.2 - EXEC CICS | 40 jours | +8-10% |
| 1.3 - TODOs | 30 jours | +5-8% |
| 1.4 - REDEFINES | 20 jours | +3-5% |
| **TOTAL** | **130 jours** | **+26-35%** |

**Gain conversion:** 82% → 92-95%
**Durée avec 3 devs:** 3 mois

---

# 🟡 PHASE 2: ROBUSTESSE & QUALITÉ

**Durée:** 2 mois
**Équipe:** 2 développeurs
**Objectif:** Passer de 92-95% à 97-98% de conversion

---

## EPIC 2.1: Support OCCURS DEPENDING ON Dynamique

**Priorité:** 🟡 HAUTE
**Impact:** +2 à +3% conversion
**Effort:** 10 jours
**Complexité:** Moyenne

### User Stories

#### US-2.1.1: Détecter OCCURS DEPENDING ON

**En tant que** développeur du convertisseur
**Je veux** détecter les tableaux avec taille dynamique
**Afin de** générer des collections Java dynamiques

**Critères d'acceptation:**
- [ ] Détection clause OCCURS ... TO ... DEPENDING ON
- [ ] Extraction de la variable de taille
- [ ] Détection des limites min/max

**Exemple:**
```cobol
01 WS-TABLE.
   05 WS-COUNT    PIC 99.
   05 WS-ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON WS-COUNT.
      10 WS-ITEM  PIC X(20).
```

**Tâches techniques:**
1. Améliorer parser OCCURS (2 jours)
2. Extraire variable DEPENDING ON (1 jour)
3. Tests unitaires (1 jour)

**Estimation:** 4 jours

---

#### US-2.1.2: Générer collections Java dynamiques

**En tant que** développeur du convertisseur
**Je veux** générer List<T> au lieu de T[] pour tableaux dynamiques
**Afin de** supporter la taille variable

**Critères d'acceptation:**
- [ ] OCCURS DEPENDING ON → List<Type>
- [ ] Méthodes add/remove/size générées
- [ ] Synchronisation avec variable compteur

**Code généré:**
```java
public class WsTable {
    private int wsCount = 0;
    private List<String> wsItems = new ArrayList<>(100); // Capacité initiale

    public void addWsItem(String item) {
        if (wsItems.size() < 100) { // Max 100
            wsItems.add(item);
            wsCount = wsItems.size();
        } else {
            throw new IllegalStateException("Table full (max 100 items)");
        }
    }

    public void removeWsItem(int index) {
        if (index >= 0 && index < wsItems.size()) {
            wsItems.remove(index);
            wsCount = wsItems.size();
        }
    }

    public int getWsCount() {
        return wsCount;
    }

    public List<String> getWsItems() {
        return Collections.unmodifiableList(wsItems);
    }
}
```

**Tâches techniques:**
1. Modifier EntityGenerator pour List<T> (2 jours)
2. Générer méthodes add/remove (1 jour)
3. Synchroniser variable compteur (1 jour)
4. Tests unitaires (2 jours)

**Estimation:** 6 jours

---

#### Résumé EPIC 2.1

**Stories:** 2
**Effort total:** 10 jours
**Tests:** 3 jours inclus

---

## EPIC 2.2: EVALUATE ALSO Avancé

**Priorité:** 🟡 HAUTE
**Impact:** +1 à +2% conversion
**Effort:** 8 jours
**Complexité:** Moyenne

### User Stories

#### US-2.2.1: Support EVALUATE ALSO avec >2 expressions

**En tant que** développeur du convertisseur
**Je veux** supporter EVALUATE avec 3+ expressions combinées
**Afin de** générer des conditions complexes correctement

**Critères d'acceptation:**
- [ ] EVALUATE expr1 ALSO expr2 ALSO expr3 ... ALSO exprN
- [ ] Génération de conditions AND combinées
- [ ] Support ANY pour expressions wildcards

**Exemple:**
```cobol
EVALUATE STATUS ALSO ERROR-CODE ALSO REGION
   WHEN 'A' ALSO '01' ALSO 'EAST'
      PERFORM PROCESS-EAST-ACTIVE
   WHEN 'I' ALSO ANY ALSO 'WEST'
      PERFORM PROCESS-WEST-INACTIVE
   WHEN OTHER
      PERFORM PROCESS-DEFAULT
END-EVALUATE.
```

**Code généré:**
```java
// COBOL: EVALUATE STATUS ALSO ERROR-CODE ALSO REGION
if (record.getStatus().equals("A") &&
    record.getErrorCode().equals("01") &&
    record.getRegion().equals("EAST")) {

    processEastActive(record);

} else if (record.getStatus().equals("I") &&
           record.getRegion().equals("WEST")) {
    // ANY pour ERROR-CODE: pas de condition
    processWestInactive(record);

} else {
    processDefault(record);
}
```

**Tâches techniques:**
1. Parser EVALUATE ALSO avec N expressions (2 jours)
2. Générer conditions AND multiples (2 jours)
3. Gérer ANY (wildcards) (1 jour)
4. Tests unitaires (2 jours)

**Estimation:** 7 jours

---

#### Résumé EPIC 2.2

**Stories:** 1
**Effort total:** 7 jours (arrondi à 8 jours)
**Tests:** 2 jours inclus

---

## EPIC 2.3: INSPECT Combiné (TALLYING + REPLACING)

**Priorité:** 🟡 HAUTE
**Impact:** +1 à +2% conversion
**Effort:** 5 jours
**Complexité:** Moyenne

### User Stories

#### US-2.3.1: Support INSPECT avec opérations multiples

**En tant que** développeur du convertisseur
**Je veux** supporter INSPECT avec TALLYING et REPLACING combinés
**Afin de** générer du code Java effectuant les deux opérations

**Critères d'acceptation:**
- [ ] INSPECT field TALLYING ... REPLACING ...
- [ ] Exécution séquentielle des opérations
- [ ] Gestion des modes (ALL, LEADING, FIRST)

**Exemple:**
```cobol
INSPECT WS-INPUT
   TALLYING WS-COUNT FOR ALL 'A'
   REPLACING ALL 'B' BY 'C'
   REPLACING LEADING SPACES BY ZEROS.
```

**Code généré:**
```java
// COBOL: INSPECT WS-INPUT
String inspectStr = record.getWsInput();

// TALLYING FOR ALL 'A'
int tallyCount = 0;
for (int i = 0; i < inspectStr.length(); i++) {
    if (inspectStr.charAt(i) == 'A') {
        tallyCount++;
    }
}
record.setWsCount(String.valueOf(tallyCount));

// REPLACING ALL 'B' BY 'C'
inspectStr = inspectStr.replace('B', 'C');

// REPLACING LEADING SPACES BY ZEROS
StringBuilder sb = new StringBuilder(inspectStr);
for (int i = 0; i < sb.length(); i++) {
    if (sb.charAt(i) == ' ') {
        sb.setCharAt(i, '0');
    } else {
        break; // Stop at first non-space
    }
}
inspectStr = sb.toString();

record.setWsInput(inspectStr);
```

**Tâches techniques:**
1. Parser INSPECT avec multiples clauses (2 jours)
2. Générer code séquentiel (2 jours)
3. Tests unitaires (combinaisons) (2 jours)

**Estimation:** 6 jours (arrondi à 5 jours avec optimisation)

---

#### Résumé EPIC 2.3

**Stories:** 1
**Effort total:** 5 jours
**Tests:** 2 jours inclus

---

## EPIC 2.4: SORT Gros Volumes (External Sort)

**Priorité:** 🟡 HAUTE
**Impact:** +2 à +3% conversion
**Effort:** 12 jours
**Complexité:** Élevée

### User Stories

#### US-2.4.1: Détecter SORT et analyser volumes

**En tant que** développeur du convertisseur
**Je veux** détecter les SORT statements et estimer les volumes
**Afin de** choisir la stratégie de tri appropriée

**Critères d'acceptation:**
- [ ] Détection SORT statements
- [ ] Extraction des clés de tri
- [ ] Estimation de la taille des fichiers (commentaires/métadonnées)

**Tâches techniques:**
1. Améliorer parser SORT (1 jour)
2. Extraire clés de tri (1 jour)
3. Tests unitaires (1 jour)

**Estimation:** 3 jours

---

#### US-2.4.2: Implémenter tri par chunks (external sort)

**En tant que** développeur du convertisseur
**Je veux** générer du code de tri par chunks pour gros volumes
**Afin d'** éviter les OutOfMemoryError

**Critères d'acceptation:**
- [ ] Tri en mémoire pour petits fichiers (<10K records)
- [ ] Tri par chunks pour gros fichiers (>10K records)
- [ ] Utilisation de fichiers temporaires
- [ ] Merge final des chunks triés

**Code généré:**
```java
@Bean
public Step sortStep(JobRepository jobRepository,
                    PlatformTransactionManager transactionManager) {
    return new StepBuilder("sortStep", jobRepository)
        .<CustomerRecord, CustomerRecord>chunk(1000, transactionManager)
        .reader(unsortedFileReader())
        .processor(new ItemProcessor<CustomerRecord, CustomerRecord>() {
            private List<CustomerRecord> buffer = new ArrayList<>(1000);

            @Override
            public CustomerRecord process(CustomerRecord item) throws Exception {
                buffer.add(item);

                // Tri du buffer quand plein
                if (buffer.size() >= 1000) {
                    buffer.sort(Comparator.comparing(CustomerRecord::getCustId));
                    writeToTempFile(buffer);
                    buffer.clear();
                }

                return null; // Pas d'écriture directe
            }
        })
        .writer(items -> {}) // No-op writer
        .build();
}

@Bean
public Step mergeStep(JobRepository jobRepository,
                     PlatformTransactionManager transactionManager) {
    // Merge des fichiers temporaires triés
    return new StepBuilder("mergeStep", jobRepository)
        .tasklet((contribution, chunkContext) -> {
            mergeSortedTempFiles();
            return RepeatStatus.FINISHED;
        })
        .build();
}
```

**Tâches techniques:**
1. Créer `ExternalSortGenerator` (3 jours)
2. Implémenter stratégie de tri (chunk-based) (3 jours)
3. Implémenter merge de chunks (2 jours)
4. Tests unitaires (petits et gros volumes) (3 jours)

**Estimation:** 11 jours (arrondi à 12 jours)

---

#### Résumé EPIC 2.4

**Stories:** 2
**Effort total:** 14 jours (optimisé à 12 jours)
**Tests:** 4 jours inclus

---

## EPIC 2.5: Suite de Tests Complète

**Priorité:** 🟡 HAUTE
**Impact:** Qualité & Robustesse
**Effort:** 26 jours
**Complexité:** Élevée

### User Stories

#### US-2.5.1: Tests unitaires ProjectGenerator

**En tant que** développeur
**Je veux** des tests unitaires pour ProjectGenerator
**Afin de** garantir la génération correcte de la structure Maven

**Critères d'acceptation:**
- [ ] Tests de génération pom.xml
- [ ] Tests de génération application.properties
- [ ] Tests de génération structure de répertoires
- [ ] Tests de génération README, .gitignore

**Tâches techniques:**
1. Créer `ProjectGeneratorTest` (3 jours)
2. Tests génération pom.xml (2 jours)
3. Tests génération fichiers config (2 jours)
4. Tests génération structure (2 jours)

**Estimation:** 9 jours

---

#### US-2.5.2: Tests d'intégration Web Controller

**En tant que** développeur
**Je veux** des tests d'intégration pour le contrôleur web
**Afin de** valider l'API de conversion

**Critères d'acceptation:**
- [ ] Tests upload fichier COBOL
- [ ] Tests conversion et récupération code Java
- [ ] Tests téléchargement projet ZIP
- [ ] Tests gestion d'erreurs

**Tâches techniques:**
1. Créer `ConversionControllerIntegrationTest` (2 jours)
2. Tests upload et conversion (2 jours)
3. Tests download ZIP (1 jour)
4. Tests gestion erreurs (1 jour)

**Estimation:** 6 jours

---

#### US-2.5.3: Tests End-to-End

**En tant que** développeur
**Je veux** des tests E2E complets (COBOL → Java → exécution)
**Afin de** valider le processus complet de conversion

**Critères d'acceptation:**
- [ ] 10+ programmes COBOL réels testés
- [ ] Compilation du code Java généré
- [ ] Exécution des jobs Spring Batch
- [ ] Validation des résultats de sortie

**Tâches techniques:**
1. Collecter 10 programmes COBOL réels (1 jour)
2. Créer framework de tests E2E (2 jours)
3. Implémenter tests conversion complète (3 jours)
4. Validation des résultats (2 jours)

**Estimation:** 8 jours

---

#### US-2.5.4: Tests de Performance

**En tant que** développeur
**Je veux** des tests de performance et de charge
**Afin de** valider le comportement sur gros volumes

**Critères d'acceptation:**
- [ ] Tests conversion de gros programmes COBOL (>10K lignes)
- [ ] Tests de charge (100+ fichiers en parallèle)
- [ ] Mesure de la vitesse de conversion
- [ ] Profiling mémoire

**Tâches techniques:**
1. Créer framework de tests performance (2 jours)
2. Tests gros programmes (1 jour)
3. Tests de charge (1 jour)
4. Profiling et optimisation (2 jours)

**Estimation:** 6 jours

---

#### Résumé EPIC 2.5

**Stories:** 4
**Effort total:** 29 jours (optimisé à 26 jours)

---

## Résumé Phase 2

| EPIC | Effort | Impact |
|------|--------|--------|
| 2.1 - OCCURS DEPENDING ON | 10 jours | +2-3% |
| 2.2 - EVALUATE ALSO | 8 jours | +1-2% |
| 2.3 - INSPECT Combiné | 5 jours | +1-2% |
| 2.4 - SORT Gros Volumes | 12 jours | +2-3% |
| 2.5 - Tests Complets | 26 jours | Qualité |
| **TOTAL** | **61 jours** | **+6-10%** |

**Gain conversion:** 92-95% → 97-98%
**Durée avec 2 devs:** 2 mois

---

# 🟢 PHASE 3: EXCELLENCE & OPTIMISATION

**Durée:** 1.5 mois
**Équipe:** 2 développeurs
**Objectif:** Passer de 97-98% à 99-100% de conversion

---

## EPIC 3.1: Refactorisation Automatique GO TO

**Priorité:** 🟢 MOYENNE
**Impact:** +1 à +2% conversion
**Effort:** 15 jours
**Complexité:** Très élevée

### User Stories

#### US-3.1.1: Analyser flux de contrôle

**En tant que** développeur du convertisseur
**Je veux** analyser le flux de contrôle des programmes COBOL
**Afin de** détecter les patterns GO TO refactorisables

**Critères d'acceptation:**
- [ ] Détection de tous les GO TO statements
- [ ] Construction du graphe de flux de contrôle (CFG)
- [ ] Identification des patterns (if-then-goto, goto-chain, etc.)
- [ ] Rapport des GO TO non refactorisables

**Tâches techniques:**
1. Créer `ControlFlowAnalyzer` (3 jours)
2. Construire CFG (Control Flow Graph) (3 jours)
3. Détecter patterns GO TO (2 jours)
4. Tests unitaires (2 jours)

**Estimation:** 10 jours

---

#### US-3.1.2: Refactoriser GO TO en structures de contrôle

**En tant que** développeur du convertisseur
**Je veux** transformer les GO TO en if/else/while
**Afin de** générer du code Java structuré

**Critères d'acceptation:**
- [ ] Pattern IF-THEN-GOTO → if-else
- [ ] Pattern GOTO-CHAIN → méthodes séquentielles
- [ ] Pattern GOTO-LOOP → while/for loops
- [ ] Préservation de la sémantique

**Exemple:**

```cobol
PARA-010.
   IF CONDITION-1
      GO TO PARA-030
   END-IF.
   PERFORM PARA-020.
   GO TO PARA-040.

PARA-020.
   MOVE X TO Y.

PARA-030.
   MOVE A TO B.

PARA-040.
   STOP RUN.
```

**Code refactorisé:**
```java
public void para010() {
    if (condition1) {
        para030Logic(); // Inline de PARA-030
    } else {
        para020Logic(); // Inline de PARA-020
    }
    para040Logic(); // Inline de PARA-040
}

private void para020Logic() {
    record.setY(record.getX());
}

private void para030Logic() {
    record.setB(record.getA());
}

private void para040Logic() {
    // STOP RUN
}
```

**Tâches techniques:**
1. Créer `GotoRefactorer` (3 jours)
2. Implémenter patterns de refactoring (4 jours)
3. Valider préservation sémantique (2 jours)
4. Tests unitaires (3 jours)

**Estimation:** 12 jours

---

#### Résumé EPIC 3.1

**Stories:** 2
**Effort total:** 22 jours (optimisé à 15 jours avec simplification)
**Tests:** 5 jours inclus

---

## EPIC 3.2: Génération Améliorée

**Priorité:** 🟢 MOYENNE
**Impact:** Qualité du code généré
**Effort:** 25 jours
**Complexité:** Moyenne

### User Stories

#### US-3.2.1: Validation JPA et Relations

**En tant que** développeur du convertisseur
**Je veux** générer des entités JPA avec validation et relations
**Afin de** produire du code conforme aux best practices

**Critères d'acceptation:**
- [ ] Annotations de validation (@NotNull, @Size, @Pattern, etc.)
- [ ] Relations entre entités (@OneToMany, @ManyToOne, etc.)
- [ ] Audit trail (Hibernate Envers)
- [ ] Contraintes de base de données (@UniqueConstraint, etc.)

**Code généré amélioré:**
```java
@Entity
@Table(name = "CUSTOMER",
       uniqueConstraints = @UniqueConstraint(columnNames = {"custEmail"}),
       indexes = {
           @Index(name = "idx_cust_name", columnList = "custName")
       })
@Audited // Hibernate Envers
public class Customer {

    @Id
    @Column(name = "cust_id", length = 10, nullable = false)
    @NotNull(message = "Customer ID cannot be null")
    @Size(min = 1, max = 10, message = "Customer ID must be 1-10 characters")
    private String custId;

    @Column(name = "cust_name", length = 30, nullable = false)
    @NotBlank(message = "Customer name is required")
    @Size(max = 30)
    private String custName;

    @Column(name = "cust_email", length = 50, unique = true)
    @Email(message = "Invalid email format")
    private String custEmail;

    @Column(name = "cust_balance", precision = 15, scale = 2)
    @DecimalMin(value = "0.0", message = "Balance cannot be negative")
    private BigDecimal custBalance;

    @Enumerated(EnumType.STRING)
    @Column(name = "cust_status", length = 1)
    @Pattern(regexp = "[AIPS]", message = "Status must be A, I, P, or S")
    private String custStatus;

    // Relations
    @OneToMany(mappedBy = "customer", cascade = CascadeType.ALL)
    private List<Order> orders = new ArrayList<>();

    // Audit fields (Envers)
    @CreatedDate
    @Column(name = "created_date", nullable = false, updatable = false)
    private LocalDateTime createdDate;

    @LastModifiedDate
    @Column(name = "modified_date")
    private LocalDateTime modifiedDate;

    // Getters/Setters
}
```

**Tâches techniques:**
1. Améliorer EntityGenerator (validation) (3 jours)
2. Détecter relations entre records COBOL (3 jours)
3. Générer relations JPA (2 jours)
4. Support Hibernate Envers (2 jours)
5. Tests unitaires (2 jours)

**Estimation:** 12 jours

---

#### US-3.2.2: Job Configuration Avancée

**En tant que** développeur du convertisseur
**Je veux** générer des configurations Spring Batch avancées
**Afin de** produire des jobs robustes et performants

**Critères d'acceptation:**
- [ ] Job parameters dynamiques
- [ ] Chunk size adaptatif
- [ ] Partitioning et parallel steps
- [ ] Retry/Skip policies avancées
- [ ] Job listeners avec métriques

**Code généré amélioré:**
```java
@Configuration
public class CustomerJobConfig {

    @Value("${app.batch.chunk-size:100}")
    private int chunkSize;

    @Bean
    public Job customerJob(JobRepository jobRepository,
                          Step customerStep,
                          Step reportStep) {
        return new JobBuilder("customerJob", jobRepository)
            .start(customerStep)
            .next(reportStep)
            .listener(jobExecutionListener())
            .build();
    }

    @Bean
    public Step customerStep(JobRepository jobRepository,
                            PlatformTransactionManager transactionManager,
                            FlatFileItemReader<CustomerRecord> reader,
                            CustomerProcessor processor,
                            FlatFileItemWriter<CustomerRecord> writer) {
        return new StepBuilder("customerStep", jobRepository)
            .<CustomerRecord, CustomerRecord>chunk(chunkSize, transactionManager)
            .reader(reader)
            .processor(processor)
            .writer(writer)
            .faultTolerant()
            .skipLimit(10)
            .skip(ValidationException.class)
            .retryLimit(3)
            .retry(DeadlockLoserDataAccessException.class)
            .listener(stepExecutionListener())
            .build();
    }

    // Partitioning pour parallélisation
    @Bean
    public Step partitionedStep(JobRepository jobRepository,
                               Step customerStep,
                               Partitioner partitioner) {
        return new StepBuilder("partitionedStep", jobRepository)
            .partitioner("customerStep", partitioner)
            .step(customerStep)
            .gridSize(4) // 4 threads
            .taskExecutor(taskExecutor())
            .build();
    }

    @Bean
    public JobExecutionListener jobExecutionListener() {
        return new JobExecutionListener() {
            @Override
            public void beforeJob(JobExecution jobExecution) {
                logger.info("Job started: {}", jobExecution.getJobInstance().getJobName());
            }

            @Override
            public void afterJob(JobExecution jobExecution) {
                logger.info("Job completed: {} in {} ms",
                    jobExecution.getStatus(),
                    jobExecution.getEndTime().getTime() - jobExecution.getStartTime().getTime());
            }
        };
    }
}
```

**Tâches techniques:**
1. Améliorer JobConfigGenerator (parameters) (2 jours)
2. Support chunk size adaptatif (2 jours)
3. Générer partitioning/parallel steps (3 jours)
4. Retry/Skip policies (2 jours)
5. Job listeners et métriques (2 jours)
6. Tests unitaires (2 jours)

**Estimation:** 13 jours

---

#### US-3.2.3: Tests Améliorés

**En tant que** développeur du convertisseur
**Je veux** générer des tests complets et réalistes
**Afin de** faciliter la validation du code généré

**Critères d'acceptation:**
- [ ] Tests d'intégration avec base de données réelle
- [ ] Tests de charge (JMeter/Gatling)
- [ ] Mocks de dépendances externes
- [ ] Tests de scénarios métier complets

**Tâches techniques:**
1. Améliorer TestGenerator (tests DB) (2 jours)
2. Générer tests de charge (2 jours)
3. Générer mocks (1 jour)
4. Tests unitaires (1 jour)

**Estimation:** 6 jours

---

#### Résumé EPIC 3.2

**Stories:** 3
**Effort total:** 31 jours (optimisé à 25 jours)
**Tests:** 5 jours inclus

---

## EPIC 3.3: Support CI/CD

**Priorité:** 🟢 MOYENNE
**Impact:** DevOps & Automatisation
**Effort:** 10 jours
**Complexité:** Moyenne

### User Stories

#### US-3.3.1: Générer workflows CI/CD

**En tant que** développeur du convertisseur
**Je veux** générer des workflows CI/CD pour les projets
**Afin de** automatiser build, tests, et déploiement

**Critères d'acceptation:**
- [ ] GitHub Actions workflow
- [ ] GitLab CI pipeline
- [ ] Jenkins pipeline
- [ ] Docker Compose pour environnements de test

**Exemple GitHub Actions généré:**
```yaml
# .github/workflows/build.yml
name: Build and Test

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    runs-on: ubuntu-latest

    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_DB: testdb
          POSTGRES_USER: test
          POSTGRES_PASSWORD: test
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 5432:5432

    steps:
    - uses: actions/checkout@v3

    - name: Set up JDK 17
      uses: actions/setup-java@v3
      with:
        java-version: '17'
        distribution: 'temurin'
        cache: maven

    - name: Build with Maven
      run: mvn clean install -DskipTests

    - name: Run unit tests
      run: mvn test

    - name: Run integration tests
      run: mvn verify -Pintegration-tests
      env:
        SPRING_DATASOURCE_URL: jdbc:postgresql://localhost:5432/testdb
        SPRING_DATASOURCE_USERNAME: test
        SPRING_DATASOURCE_PASSWORD: test

    - name: Build Docker image
      run: docker build -t customer-batch:${{ github.sha }} .

    - name: Upload coverage to Codecov
      uses: codecov/codecov-action@v3
      with:
        files: ./target/site/jacoco/jacoco.xml
```

**Exemple GitLab CI généré:**
```yaml
# .gitlab-ci.yml
stages:
  - build
  - test
  - deploy

variables:
  MAVEN_OPTS: "-Dmaven.repo.local=$CI_PROJECT_DIR/.m2/repository"

cache:
  paths:
    - .m2/repository

build:
  stage: build
  image: maven:3.9-openjdk-17
  script:
    - mvn clean compile
  artifacts:
    paths:
      - target/

test:
  stage: test
  image: maven:3.9-openjdk-17
  services:
    - postgres:15
  variables:
    POSTGRES_DB: testdb
    POSTGRES_USER: test
    POSTGRES_PASSWORD: test
    SPRING_DATASOURCE_URL: jdbc:postgresql://postgres:5432/testdb
  script:
    - mvn test
    - mvn verify -Pintegration-tests
  coverage: '/Total.*?([0-9]{1,3})%/'
  artifacts:
    reports:
      junit:
        - target/surefire-reports/TEST-*.xml
        - target/failsafe-reports/TEST-*.xml

deploy:
  stage: deploy
  image: docker:latest
  services:
    - docker:dind
  script:
    - docker build -t customer-batch:$CI_COMMIT_SHA .
    - docker tag customer-batch:$CI_COMMIT_SHA customer-batch:latest
  only:
    - main
```

**Exemple Docker Compose généré:**
```yaml
# docker-compose.yml
version: '3.8'

services:
  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: batchdb
      POSTGRES_USER: batch
      POSTGRES_PASSWORD: batch123
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U batch"]
      interval: 10s
      timeout: 5s
      retries: 5

  app:
    build: .
    depends_on:
      postgres:
        condition: service_healthy
    environment:
      SPRING_DATASOURCE_URL: jdbc:postgresql://postgres:5432/batchdb
      SPRING_DATASOURCE_USERNAME: batch
      SPRING_DATASOURCE_PASSWORD: batch123
      SPRING_BATCH_JOB_ENABLED: "true"
    volumes:
      - ./data:/app/data
    ports:
      - "8080:8080"

volumes:
  postgres_data:
```

**Tâches techniques:**
1. Créer `CiCdGenerator` (2 jours)
2. Générer GitHub Actions (2 jours)
3. Générer GitLab CI (2 jours)
4. Générer Jenkins pipeline (2 jours)
5. Générer Docker Compose (1 jour)
6. Tests (1 jour)

**Estimation:** 10 jours

---

#### Résumé EPIC 3.3

**Stories:** 1
**Effort total:** 10 jours
**Tests:** 1 jour inclus

---

## EPIC 3.4: UI Web Améliorée

**Priorité:** 🟢 BASSE
**Impact:** Expérience Utilisateur
**Effort:** 15 jours
**Complexité:** Moyenne

### User Stories

#### US-3.4.1: Upload multiple fichiers (drag & drop)

**En tant qu'** utilisateur
**Je veux** uploader plusieurs fichiers COBOL en drag & drop
**Afin de** convertir un batch de programmes rapidement

**Critères d'acceptation:**
- [ ] Drag & drop de fichiers
- [ ] Upload multiple simultané
- [ ] Barre de progression
- [ ] Aperçu des fichiers avant conversion

**Tâches techniques:**
1. Frontend: Zone drag & drop (2 jours)
2. Backend: Endpoint upload multiple (1 jour)
3. Tests (1 jour)

**Estimation:** 4 jours

---

#### US-3.4.2: Conversion en temps réel (WebSocket)

**En tant qu'** utilisateur
**Je veux** voir la progression de la conversion en temps réel
**Afin de** suivre l'avancement

**Critères d'acceptation:**
- [ ] WebSocket pour communication temps réel
- [ ] Affichage progression (%)
- [ ] Logs de conversion en direct
- [ ] Notification de fin

**Tâches techniques:**
1. Backend: WebSocket configuration (2 jours)
2. Frontend: Affichage temps réel (2 jours)
3. Tests (1 jour)

**Estimation:** 5 jours

---

#### US-3.4.3: Comparaison COBOL/Java côte à côte

**En tant qu'** utilisateur
**Je veux** comparer le code COBOL et Java côte à côte
**Afin de** valider la conversion

**Critères d'acceptation:**
- [ ] Affichage côte à côte (split view)
- [ ] Synchronisation du scroll
- [ ] Coloration syntaxique
- [ ] Highlighting des blocs correspondants

**Tâches techniques:**
1. Frontend: Split view editor (2 jours)
2. Backend: Mapping COBOL↔Java (1 jour)
3. Tests (1 jour)

**Estimation:** 4 jours

---

#### US-3.4.4: Export et historique

**En tant qu'** utilisateur
**Je veux** télécharger les projets et consulter l'historique
**Afin de** retrouver mes conversions précédentes

**Critères d'acceptation:**
- [ ] Export projet complet en ZIP
- [ ] Export code Java uniquement
- [ ] Historique des conversions
- [ ] Recherche dans l'historique

**Tâches techniques:**
1. Backend: Génération ZIP (1 jour)
2. Backend: Sauvegarde historique (1 jour)
3. Frontend: Interface historique (2 jours)
4. Tests (1 jour)

**Estimation:** 5 jours

---

#### Résumé EPIC 3.4

**Stories:** 4
**Effort total:** 18 jours (optimisé à 15 jours)
**Tests:** 4 jours inclus

---

## Résumé Phase 3

| EPIC | Effort | Impact |
|------|--------|--------|
| 3.1 - Refactoring GO TO | 15 jours | +1-2% |
| 3.2 - Génération Améliorée | 25 jours | Qualité |
| 3.3 - Support CI/CD | 10 jours | DevOps |
| 3.4 - UI Web Améliorée | 15 jours | UX |
| **TOTAL** | **65 jours** | **+1-2%** |

**Gain conversion:** 97-98% → 99-100%
**Durée avec 2 devs:** 1.5 mois

---

# 📊 RÉCAPITULATIF GLOBAL

## Métriques Finales Attendues

| Métrique | Actuel | Après Phase 1 | Après Phase 2 | Après Phase 3 |
|----------|--------|---------------|---------------|---------------|
| **Taux conversion** | 76-82% | 92-95% | 97-98% | 99-100% |
| **Support EXEC SQL** | 0% | 95% | 95% | 95% |
| **Support EXEC CICS** | 0% | 90% | 90% | 90% |
| **TODOs** | 105 | 0 | 0 | 0 |
| **REDEFINES complexes** | 70% | 95% | 95% | 95% |
| **Couverture tests** | 80% | 85% | 95% | 98% |
| **Tests E2E** | 0 | 10+ | 50+ | 100+ |

## Effort et Planning

### Vue d'ensemble

| Phase | Durée | Équipe | Effort Total | Gain Conversion |
|-------|-------|--------|--------------|-----------------|
| **Phase 1** | 3 mois | 2-3 devs | 130 jours | +10 à +13% |
| **Phase 2** | 2 mois | 2 devs | 61 jours | +5 à +6% |
| **Phase 3** | 1.5 mois | 2 devs | 65 jours | +2 à +3% |
| **TOTAL** | **6.5 mois** | | **256 jours** | **+17 à +22%** |

### Planning Gantt (Simplifié)

```
Mois 1-3: PHASE 1 (Fondations Critiques)
├─ EPIC 1.1: EXEC SQL → JPA (40j)
├─ EPIC 1.2: EXEC CICS → REST (40j) [parallèle]
├─ EPIC 1.3: TODOs (30j)
└─ EPIC 1.4: REDEFINES (20j) [parallèle avec 1.3]

Mois 4-5: PHASE 2 (Robustesse & Qualité)
├─ EPIC 2.1: OCCURS DEPENDING ON (10j)
├─ EPIC 2.2: EVALUATE ALSO (8j) [parallèle]
├─ EPIC 2.3: INSPECT Combiné (5j)
├─ EPIC 2.4: SORT (12j) [parallèle]
└─ EPIC 2.5: Tests Complets (26j)

Mois 6-7.5: PHASE 3 (Excellence)
├─ EPIC 3.1: Refactoring GO TO (15j)
├─ EPIC 3.2: Génération Améliorée (25j)
├─ EPIC 3.3: CI/CD (10j) [parallèle]
└─ EPIC 3.4: UI Web (15j) [parallèle]
```

## Dépendances Entre EPICs

```
EPIC 1.1 (EXEC SQL)
  └─> Requis pour: Tests E2E (EPIC 2.5)

EPIC 1.2 (EXEC CICS)
  └─> Requis pour: Tests E2E (EPIC 2.5)

EPIC 1.3 (TODOs)
  └─> Requis pour: Tous les autres EPICs

EPIC 1.4 (REDEFINES)
  └─> Requis pour: Génération Améliorée (EPIC 3.2)

EPIC 2.5 (Tests Complets)
  └─> Requis pour: Validation de tous les EPICs

EPIC 3.1 (Refactoring GO TO)
  └─> Optionnel, indépendant

EPIC 3.2 (Génération Améliorée)
  └─> Dépend de: EPIC 1.4

EPIC 3.3 (CI/CD)
  └─> Indépendant, peut être fait en parallèle

EPIC 3.4 (UI Web)
  └─> Indépendant, peut être fait en parallèle
```

## Risques et Mitigation

| Risque | Probabilité | Impact | Mitigation |
|--------|-------------|--------|------------|
| Complexité EXEC SQL/CICS sous-estimée | Moyenne | Élevé | Preuve de concept (PoC) dès début Phase 1 |
| Tests E2E révèlent bugs majeurs | Moyenne | Moyen | Tests incrémentaux dès Phase 1 |
| Performance insuffisante (SORT) | Faible | Moyen | Profiling et benchmarks pendant Phase 2 |
| Refactoring GO TO trop complexe | Élevée | Faible | Phase 3 optionnelle, peut être simplifiée |
| Incompatibilités bases de données | Moyenne | Moyen | Tests multi-DB dès Phase 1 |

## Livrables par Phase

### Phase 1
- [ ] Module EXEC SQL → JPA fonctionnel
- [ ] Module EXEC CICS → REST fonctionnel
- [ ] Code sans TODOs
- [ ] Support REDEFINES complexes
- [ ] Documentation technique mise à jour
- [ ] 10+ programmes COBOL de test convertis

### Phase 2
- [ ] Support OCCURS DEPENDING ON
- [ ] Support EVALUATE ALSO avancé
- [ ] Support INSPECT combiné
- [ ] Support SORT gros volumes
- [ ] Suite de tests complète (unit, integration, E2E, performance)
- [ ] Rapport de couverture 95%+
- [ ] 50+ programmes COBOL de test convertis

### Phase 3
- [ ] Module refactoring GO TO
- [ ] Génération avancée (validation JPA, relations, audit)
- [ ] Workflows CI/CD (GitHub Actions, GitLab CI, Jenkins)
- [ ] UI Web améliorée (drag & drop, temps réel, comparaison)
- [ ] Documentation utilisateur complète
- [ ] 100+ programmes COBOL de test convertis
- [ ] Guide de migration COBOL→Java

## Critères de Succès

### Critères Techniques
- [ ] **Taux de conversion ≥ 99%** sur panel de 100+ programmes COBOL réels
- [ ] **Couverture de tests ≥ 95%**
- [ ] **0 TODOs** dans le code
- [ ] **Performance:** Conversion d'un programme COBOL de 10K lignes en <30 secondes
- [ ] **Compilation:** 100% du code Java généré compile sans erreur
- [ ] **Exécution:** 95%+ des jobs Spring Batch générés s'exécutent avec succès

### Critères Fonctionnels
- [ ] Support EXEC SQL (SELECT, INSERT, UPDATE, DELETE, curseurs)
- [ ] Support EXEC CICS (READ, WRITE, REWRITE, DELETE, SEND, RECEIVE)
- [ ] Support REDEFINES multiples et complexes
- [ ] Support tableaux dynamiques (OCCURS DEPENDING ON)
- [ ] Support tri de gros volumes (external sort)

### Critères Qualité
- [ ] Code généré respecte les conventions Java
- [ ] Code généré utilise les best practices Spring Boot/Batch
- [ ] Documentation complète (technique + utilisateur)
- [ ] Processus CI/CD automatisé
- [ ] UI web intuitive et réactive

## Recommandations Stratégiques

### Option 1: Approche Minimale (Phase 1 Uniquement)
**Durée:** 3 mois
**Gain:** 82% → 92-95%
**Recommandée pour:** Projets avec contraintes de temps/budget
**Avantages:**
- ROI rapide
- Risque faible
- Couvre 95% des besoins réels

**Inconvénients:**
- Pas de support gros volumes (SORT)
- Tests limités
- Pas de CI/CD automatisé

### Option 2: Approche Complète (Phases 1+2+3)
**Durée:** 6.5 mois
**Gain:** 82% → 99-100%
**Recommandée pour:** Projets d'entreprise critiques
**Avantages:**
- Outil production-ready
- Couverture maximale
- Qualité industrielle

**Inconvénients:**
- Investissement important
- Délai plus long

### Option 3: Approche Hybride (Phases 1+2)
**Durée:** 5 mois
**Gain:** 82% → 97-98%
**Recommandée pour:** Équilibre coût/bénéfice
**Avantages:**
- Excellent taux de conversion
- Tests complets
- Coût maîtrisé

**Inconvénients:**
- Pas de refactoring GO TO automatique
- UI web basique

---

## 📝 CONCLUSION

Ce plan détaillé permet d'atteindre **99-100% de taux de conversion automatique** en 6.5 mois avec une équipe de 2-3 développeurs.

**Prochaines étapes:**
1. Valider l'approche stratégique (Option 1, 2, ou 3)
2. Constituer l'équipe de développement
3. Créer les POCs pour EXEC SQL et EXEC CICS
4. Démarrer Phase 1 - EPIC 1.1

**Contact:**
Pour questions ou clarifications sur ce plan, contactez l'équipe de développement.

---

**Document créé le:** 08 janvier 2026
**Version:** 1.0
**Auteur:** Équipe de développement COBOL→Java Translator
**Prochaine révision:** À la fin de Phase 1
