# Fichiers de Test pour CUSTPROC

Ce répertoire contient les fichiers de données de test pour valider la traduction du programme COBOL CUSTPROC.

## Structure des Données

Le programme COBOL CUSTPROC lit un fichier de clients avec la structure suivante:

### CUSTOMER-RECORD
| Champ        | Type           | Position | Longueur | Description                    |
|--------------|----------------|----------|----------|--------------------------------|
| CUST-ID      | PIC 9(6)       | 1-6      | 6        | Identifiant client (numérique) |
| CUST-NAME    | PIC X(30)      | 7-36     | 30       | Nom du client (alphanumérique) |
| CUST-AMOUNT  | PIC 9(7)V99 COMP-3 | 37-40    | 4        | Montant en centimes (packed decimal) |
| CUST-DATE    | PIC 9(8)       | 41-48    | 8        | Date AAAAMMJJ                  |

**Longueur totale par enregistrement**: 48 caractères (+ newline)

## Fichiers de Test

### 1. `customers.dat` (Format COBOL)
Fichier de données au format position fixe pour le programme COBOL original.

**Format par ligne** (48 caractères):
```
000001John Smith                      000050000020240115
|    ||                              ||       ||      |
|    ||                              ||       ||      +-- CUST-DATE (8)
|    ||                              ||       |+--------- Position 41
|    ||                              |+---------------- CUST-AMOUNT (packed, représenté en texte ici)
|    |+-------------------------------------- CUST-NAME (30, padded avec espaces)
|    +--------------------------------------- Position 7
+-------------------------------------------- CUST-ID (6)
```

**Note sur CUST-AMOUNT**:
- En COBOL réel, ce champ est en COMP-3 (packed decimal)
- Dans ce fichier de test, il est représenté en texte: `00005000` = 500.00 €
- Format: 7 chiffres entiers + 2 décimales (divisé par 100)

### 2. `customers.csv` (Format Java/CSV)
Fichier CSV équivalent pour tester le code Java Spring Batch.

**Format**:
```csv
CUST_ID,CUST_NAME,CUST_AMOUNT,CUST_DATE
000001,John Smith,500.00,20240115
```

## Données de Test

Le fichier contient **10 enregistrements clients**:

| ID     | Nom              | Montant   | Date     | HIGH VALUE? |
|--------|------------------|-----------|----------|-------------|
| 000001 | John Smith       | 500.00 €  | 15/01/24 | Non         |
| 000002 | Marie Dupont     | 1500.00 € | 16/01/24 | **Oui**     |
| 000003 | Pierre Martin    | 250.00 €  | 17/01/24 | Non         |
| 000004 | Sophie Bernard   | 2000.00 € | 18/01/24 | **Oui**     |
| 000005 | Luc Petit        | 750.00 €  | 19/01/24 | Non         |
| 000006 | Emma Dubois      | 3000.00 € | 20/01/24 | **Oui**     |
| 000007 | Thomas Moreau    | 125.00 €  | 21/01/24 | Non         |
| 000008 | Julie Laurent    | 1750.00 € | 22/01/24 | **Oui**     |
| 000009 | Antoine Simon    | 80.00 €   | 23/01/24 | Non         |
| 000010 | Claire Michel    | 2500.00 € | 24/01/24 | **Oui**     |

**Résultats Attendus**:
- Nombre total d'enregistrements traités: **10**
- Nombre de "HIGH VALUE" (montant > 1000): **5**
  - Marie Dupont (1500.00 €)
  - Sophie Bernard (2000.00 €)
  - Emma Dubois (3000.00 €)
  - Julie Laurent (1750.00 €)
  - Claire Michel (2500.00 €)

## Tests

### Test du Programme COBOL Original

```bash
# Compiler le programme COBOL (GnuCOBOL requis)
cobc -x -free simple-customer.cob

# Copier le fichier de données
cp customers.dat ./

# Exécuter
./simple-customer

# Sortie attendue:
# HIGH VALUE: Marie Dupont
# HIGH VALUE: Sophie Bernard
# HIGH VALUE: Emma Dubois
# HIGH VALUE: Julie Laurent
# HIGH VALUE: Claire Michel
# PROCESSED: 00010
```

### Test du Code Java Généré

```bash
# Aller dans le projet généré
cd ../generated-projects/customer-batch-processing

# Copier le fichier CSV
cp ../../cobol-to-java-translator/examples/customers.csv src/main/resources/data/

# Compiler et exécuter
mvn spring-boot:run

# Ou utiliser le JAR
mvn clean package
java -jar target/customer-batch-processing-1.0.0-SNAPSHOT.jar
```

## Validation des Résultats

Pour valider que la traduction est correcte, comparez:

1. **Nombre d'enregistrements traités**: Doit être 10 dans les deux cas
2. **Messages "HIGH VALUE"**: Doivent afficher les 5 mêmes clients
3. **Ordre de traitement**: Doit être identique (séquentiel du premier au dernier)

## Création de Vos Propres Données de Test

### Format COBOL (customers.dat)
Pour créer vos propres données de test au format COBOL:

```
[ID:6 chars][NAME:30 chars padded][AMOUNT:8 digits][DATE:8 digits]
```

Exemple:
```
000099Alice Johnson                   001234567820240201
```
- ID: `000099`
- Name: `Alice Johnson` + 16 espaces pour atteindre 30 caractères
- Amount: `00123456` = 1234.56 € (déplacer la virgule de 2 positions)
- Date: `78` (78e jour, probablement invalide - utilisez AAAAMMJJ valide)

### Format CSV (customers.csv)
```csv
CUST_ID,CUST_NAME,CUST_AMOUNT,CUST_DATE
000099,Alice Johnson,1234.56,20240201
```

## Notes Importantes

1. **COMP-3 (Packed Decimal)**:
   - Le format réel COBOL utilise COMP-3 qui est un format binaire compacté
   - Dans ce test simplifié, nous utilisons du texte pour faciliter la création des données
   - Une implémentation COBOL réelle nécessiterait une conversion binaire

2. **Encodage**:
   - Les fichiers sont en UTF-8
   - COBOL mainframe utilise souvent EBCDIC - conversion peut être nécessaire

3. **Fin de ligne**:
   - Unix/Linux: LF (`\n`)
   - Windows: CRLF (`\r\n`)
   - Mainframe: peut varier selon le système

## Troubleshooting

### COBOL: "File not found"
- Vérifiez que `customers.dat` est dans le même répertoire que l'exécutable
- Vérifiez le nom exact dans le SELECT: `ASSIGN TO 'customers.dat'`

### Java: "No such file or directory"
- Placez `customers.csv` dans `src/main/resources/data/`
- Vérifiez la configuration du FlatFileItemReader dans le JobConfiguration

### Nombres incorrects
- Vérifiez que CUST-AMOUNT a exactement 8 chiffres dans customers.dat
- Les 2 derniers chiffres sont les décimales (ex: `00005000` = 50.00)
