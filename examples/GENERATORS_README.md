# 📊 Générateurs de Données de Test

Ce répertoire contient des scripts Python pour générer des données de test réalistes au format mainframe pour les exemples COBOL/JCL.

## 🎯 Vue d'ensemble

| Script | Exemple associé | Fichiers générés | Nombre d'enregistrements |
|--------|----------------|------------------|--------------------------|
| `generate_customer_test_data.py` | `customer-batch.jcl` | CUSTOMER.INPUT.DATA | 100 clients |
| `generate_banking_test_data.py` | `banking-transaction.jcl` | BANK.MASTER.ACCOUNTS<br>BANK.DAILY.TRANSACTIONS | 1000 comptes<br>5000 transactions |

## 🚀 Utilisation rapide

### Générer toutes les données de test

```bash
cd examples
./generate_all_test_data.sh
```

Cela génère tous les fichiers de test dans le répertoire `test-data/`.

### Générer uniquement les données clients

```bash
python3 generate_customer_test_data.py
```

### Générer uniquement les données bancaires

```bash
python3 generate_banking_test_data.py
```

## 📁 Fichiers générés

Après exécution, le répertoire `test-data/` contient :

```
test-data/
├── CUSTOMER.INPUT.DATA          # Données clients (format binaire, 80 bytes/record)
├── customers-readable.txt       # Version texte lisible des clients
├── BANK.MASTER.ACCOUNTS         # Comptes bancaires (format binaire, 200 bytes/record)
├── BANK.DAILY.TRANSACTIONS      # Transactions (format binaire, 150 bytes/record)
├── accounts-readable.txt        # Version texte lisible des comptes
├── transactions-readable.txt    # Version texte lisible des transactions
└── statistics.txt               # Statistiques des données bancaires
```

## 🔧 Détails techniques

### 1. Générateur de données clients (`generate_customer_test_data.py`)

**Format du fichier CUSTOMER.INPUT.DATA** :
- **RECFM**: FB (Fixed Block)
- **LRECL**: 80 bytes
- **Structure** :
  - Customer ID : 6 bytes (numérique)
  - Name : 30 bytes (alphanumérique)
  - Balance : 10 bytes (numérique avec 2 décimales)
  - Filler : 34 bytes (espaces)

**Données générées** :
- ✅ 100 clients avec noms français réalistes
- ✅ Soldes aléatoires entre 0 et 99,999.99 EUR
- ✅ IDs séquentiels de 000001 à 000100

### 2. Générateur de données bancaires (`generate_banking_test_data.py`)

**Format BANK.MASTER.ACCOUNTS** (200 bytes/record) :
- MA-ACCOUNT-NUMBER : 12 bytes
- MA-CUSTOMER-NAME : 50 bytes
- MA-ACCOUNT-TYPE : 2 bytes (CK/SV/IN)
- MA-CURRENT-BALANCE : 8 bytes (COMP-3)
- MA-AVAILABLE-BALANCE : 8 bytes (COMP-3)
- MA-OVERDRAFT-LIMIT : 7 bytes (COMP-3)
- MA-LAST-TRANSACTION : 8 bytes (YYYYMMDD)
- MA-OPEN-DATE : 8 bytes (YYYYMMDD)
- MA-STATUS-CODE : 1 byte (A/F/C)
- MA-BRANCH-CODE : 6 bytes
- FILLER : 40 bytes

**Format BANK.DAILY.TRANSACTIONS** (150 bytes/record) :
- TR-TRANSACTION-ID : 16 bytes (UUID)
- TR-ACCOUNT-NUMBER : 12 bytes
- TR-TRANSACTION-TYPE : 2 bytes (DB/CR/TF)
- TR-AMOUNT : 8 bytes (COMP-3)
- TR-DEST-ACCOUNT : 12 bytes
- TR-TRANSACTION-DATE : 8 bytes (YYYYMMDD)
- TR-TRANSACTION-TIME : 6 bytes (HHMMSS)
- TR-BRANCH-CODE : 6 bytes
- TR-TELLER-ID : 8 bytes
- TR-REFERENCE : 20 bytes
- FILLER : 17 bytes

**Données générées** :
- ✅ 1000 comptes bancaires
- ✅ 5000 transactions journalières
- ✅ Format COMP-3 pour les montants
- ✅ 5 agences (BR001-BR005)
- ✅ 50 guichetiers différents

## 🧪 Tester avec le traducteur COBOL

### 1. Générer les données
```bash
./generate_all_test_data.sh
```

### 2. Uploader via l'interface web

```bash
# Lancer l'application
java -jar target/cobol-translator.jar

# Ouvrir http://localhost:9090/conversion
# Uploader banking-transaction.cob + banking-transaction.jcl
```

### 3. Exécuter le batch généré

```bash
cd generated-project
mvn clean package
mvn spring-boot:run
```

## 📊 Exemple de statistiques

```
COMPTES:
  Total comptes: 1000
  Par type: CK: 334 | SV: 333 | IN: 333
  Solde total: 24,567,890.45 EUR

TRANSACTIONS:
  Total transactions: 5000
  Par type: Débit: 1667 | Crédit: 1666 | Transfert: 1667
  Montant total: 12,345,678.90 EUR
```

## ⚙️ Personnalisation

Modifiez les constantes dans les scripts :

```python
# generate_customer_test_data.py
NUM_CUSTOMERS = 100  # Modifier ici

# generate_banking_test_data.py
NUM_ACCOUNTS = 1000      # Nombre de comptes
NUM_TRANSACTIONS = 5000  # Nombre de transactions
```

---

**Version** : 1.0.0 | **Date** : Janvier 2026
