#!/usr/bin/env python3
"""
Générateur de données de test pour le système bancaire
Génère des fichiers de transactions et de comptes maîtres au format mainframe
"""

import random
import struct
from datetime import datetime, timedelta
from decimal import Decimal
import uuid

# Configuration
NUM_ACCOUNTS = 1000
NUM_TRANSACTIONS = 5000
OUTPUT_DIR = "test-data"

# Listes de données réalistes
FIRST_NAMES = [
    "Jean", "Marie", "Pierre", "Sophie", "Luc", "Anne", "Marc", "Julie",
    "Francois", "Claire", "Michel", "Isabelle", "Philippe", "Catherine",
    "Bernard", "Sylvie", "Andre", "Monique", "Jacques", "Nicole",
    "Robert", "Francoise", "Henri", "Dominique", "Louis", "Brigitte"
]

LAST_NAMES = [
    "Martin", "Bernard", "Dubois", "Thomas", "Robert", "Richard",
    "Petit", "Durand", "Leroy", "Moreau", "Simon", "Laurent",
    "Lefebvre", "Michel", "Garcia", "David", "Bertrand", "Roux",
    "Vincent", "Fournier", "Morel", "Girard", "André", "Mercier"
]

ACCOUNT_TYPES = ["CK", "SV", "IN"]  # Checking, Savings, Investment
TRANSACTION_TYPES = ["DB", "CR", "TF"]  # Debit, Credit, Transfer
BRANCH_CODES = ["BR001", "BR002", "BR003", "BR004", "BR005"]
STATUS_CODES = ["A", "A", "A", "A", "F", "C"]  # Mostly Active

class BankingDataGenerator:
    def __init__(self):
        self.accounts = []
        self.transactions = []

    def generate_account_number(self, index):
        """Génère un numéro de compte unique de 12 chiffres"""
        return f"{index:012d}"

    def generate_customer_name(self):
        """Génère un nom de client aléatoire"""
        first = random.choice(FIRST_NAMES)
        last = random.choice(LAST_NAMES)
        name = f"{first} {last}"
        # Remove accents and non-ASCII characters
        name = name.replace('é', 'e').replace('è', 'e').replace('à', 'a')
        name = name.replace('ç', 'c').replace('ù', 'u').replace('ô', 'o')
        return name.ljust(50)[:50]

    def generate_transaction_id(self):
        """Génère un ID de transaction unique (16 caractères)"""
        return str(uuid.uuid4()).replace("-", "")[:16].upper()

    def pack_decimal(self, value, total_digits=15, decimal_places=2):
        """
        Convertit un nombre en format COMP-3 (packed decimal)
        Pour PIC 9(13)V99 COMP-3
        """
        # Multiplier par 10^decimal_places pour obtenir un entier
        int_value = int(value * (10 ** decimal_places))

        # Signe: C = positif, D = négatif
        sign = 0xC if int_value >= 0 else 0xD
        int_value = abs(int_value)

        # Convertir en string de digits
        digits_str = f"{int_value:0{total_digits}d}"

        # Packer en format COMP-3 (2 digits par byte, sauf le dernier)
        packed = bytearray()
        for i in range(0, len(digits_str), 2):
            if i + 1 < len(digits_str):
                byte = (int(digits_str[i]) << 4) | int(digits_str[i + 1])
            else:
                byte = (int(digits_str[i]) << 4) | sign
            packed.append(byte)

        # Si nombre pair de digits, ajouter le signe à la fin
        if len(digits_str) % 2 == 0:
            packed.append(sign)

        return bytes(packed)

    def generate_accounts(self):
        """Génère les comptes bancaires maîtres"""
        print(f"Génération de {NUM_ACCOUNTS} comptes bancaires...")

        for i in range(NUM_ACCOUNTS):
            account = {
                'account_number': self.generate_account_number(i + 1),
                'customer_name': self.generate_customer_name(),
                'account_type': random.choice(ACCOUNT_TYPES),
                'current_balance': Decimal(random.uniform(-500, 50000)).quantize(Decimal('0.01')),
                'available_balance': Decimal(random.uniform(-500, 50000)).quantize(Decimal('0.01')),
                'overdraft_limit': Decimal(random.choice([0, 500, 1000, 2000, 5000])).quantize(Decimal('0.01')),
                'last_transaction': self.generate_date(days_ago=random.randint(0, 90)),
                'open_date': self.generate_date(days_ago=random.randint(365, 3650)),
                'status_code': random.choice(STATUS_CODES),
                'branch_code': random.choice(BRANCH_CODES)
            }
            self.accounts.append(account)

        print(f"✓ {len(self.accounts)} comptes générés")

    def generate_date(self, days_ago=0):
        """Génère une date au format YYYYMMDD"""
        date = datetime.now() - timedelta(days=days_ago)
        return date.strftime("%Y%m%d")

    def generate_time(self):
        """Génère une heure au format HHMMSS"""
        hour = random.randint(8, 18)
        minute = random.randint(0, 59)
        second = random.randint(0, 59)
        return f"{hour:02d}{minute:02d}{second:02d}"

    def generate_transactions(self):
        """Génère les transactions journalières"""
        print(f"Génération de {NUM_TRANSACTIONS} transactions...")

        teller_ids = [f"TELL{i:04d}" for i in range(1, 51)]

        for i in range(NUM_TRANSACTIONS):
            # Sélectionner un compte existant
            account = random.choice(self.accounts)

            tran_type = random.choice(TRANSACTION_TYPES)
            amount = Decimal(random.uniform(10, 5000)).quantize(Decimal('0.01'))

            # Pour les transferts, sélectionner un compte de destination
            dest_account = "000000000000"
            if tran_type == "TF":
                dest = random.choice(self.accounts)
                dest_account = dest['account_number']

            transaction = {
                'transaction_id': self.generate_transaction_id(),
                'account_number': account['account_number'],
                'transaction_type': tran_type,
                'amount': amount,
                'dest_account': dest_account,
                'transaction_date': self.generate_date(days_ago=random.randint(0, 1)),
                'transaction_time': self.generate_time(),
                'branch_code': account['branch_code'],
                'teller_id': random.choice(teller_ids),
                'reference': f"REF{random.randint(1000000, 9999999):07d}".ljust(20)[:20]
            }
            self.transactions.append(transaction)

        print(f"✓ {len(self.transactions)} transactions générées")

    def write_master_account_file(self, filename):
        """
        Écrit le fichier maître des comptes au format COBOL
        Record length: 200 bytes
        """
        print(f"\nÉcriture du fichier {filename}...")

        with open(filename, 'wb') as f:
            for account in self.accounts:
                # MA-ACCOUNT-NUMBER (12 bytes)
                record = account['account_number'].encode('ascii')

                # MA-CUSTOMER-NAME (50 bytes)
                record += account['customer_name'].encode('ascii')

                # MA-ACCOUNT-TYPE (2 bytes)
                record += account['account_type'].encode('ascii')

                # MA-CURRENT-BALANCE (8 bytes COMP-3)
                record += self.pack_decimal(account['current_balance'], 15, 2).ljust(8, b'\x00')[:8]

                # MA-AVAILABLE-BALANCE (8 bytes COMP-3)
                record += self.pack_decimal(account['available_balance'], 15, 2).ljust(8, b'\x00')[:8]

                # MA-OVERDRAFT-LIMIT (7 bytes COMP-3)
                record += self.pack_decimal(account['overdraft_limit'], 13, 2).ljust(7, b'\x00')[:7]

                # MA-LAST-TRANSACTION (8 bytes)
                record += account['last_transaction'].encode('ascii')

                # MA-OPEN-DATE (8 bytes)
                record += account['open_date'].encode('ascii')

                # MA-STATUS-CODE (1 byte)
                record += account['status_code'].encode('ascii')

                # MA-BRANCH-CODE (6 bytes)
                record += account['branch_code'].encode('ascii')

                # FILLER (40 bytes)
                record += b' ' * 40

                # Assurer que le record fait exactement 200 bytes
                if len(record) < 200:
                    record += b' ' * (200 - len(record))
                elif len(record) > 200:
                    record = record[:200]

                f.write(record)

        print(f"✓ {len(self.accounts)} comptes écrits ({len(self.accounts) * 200} bytes)")

    def write_transaction_file(self, filename):
        """
        Écrit le fichier des transactions au format COBOL
        Record length: 150 bytes
        """
        print(f"\nÉcriture du fichier {filename}...")

        with open(filename, 'wb') as f:
            for tran in self.transactions:
                # TR-TRANSACTION-ID (16 bytes)
                record = tran['transaction_id'].encode('ascii')

                # TR-ACCOUNT-NUMBER (12 bytes)
                record += tran['account_number'].encode('ascii')

                # TR-TRANSACTION-TYPE (2 bytes)
                record += tran['transaction_type'].encode('ascii')

                # TR-AMOUNT (8 bytes COMP-3)
                record += self.pack_decimal(tran['amount'], 15, 2).ljust(8, b'\x00')[:8]

                # TR-DEST-ACCOUNT (12 bytes)
                record += tran['dest_account'].encode('ascii')

                # TR-TRANSACTION-DATE (8 bytes)
                record += tran['transaction_date'].encode('ascii')

                # TR-TRANSACTION-TIME (6 bytes)
                record += tran['transaction_time'].encode('ascii')

                # TR-BRANCH-CODE (6 bytes)
                record += tran['branch_code'].encode('ascii')

                # TR-TELLER-ID (8 bytes)
                record += tran['teller_id'].encode('ascii')

                # TR-REFERENCE (20 bytes)
                record += tran['reference'].encode('ascii')

                # FILLER (17 bytes)
                record += b' ' * 17

                # Assurer que le record fait exactement 150 bytes
                if len(record) < 150:
                    record += b' ' * (150 - len(record))
                elif len(record) > 150:
                    record = record[:150]

                f.write(record)

        print(f"✓ {len(self.transactions)} transactions écrites ({len(self.transactions) * 150} bytes)")

    def write_text_versions(self):
        """Écrit des versions texte lisibles pour vérification"""

        # Version texte des comptes
        print("\nÉcriture des versions texte...")
        with open(f"{OUTPUT_DIR}/accounts-readable.txt", 'w', encoding='utf-8') as f:
            f.write("=" * 120 + "\n")
            f.write("COMPTES BANCAIRES - VERSION LISIBLE\n")
            f.write("=" * 120 + "\n\n")
            f.write(f"{'Numéro':<15} {'Client':<30} {'Type':<6} {'Solde':>15} {'Découvert':>12} {'Statut':<8} {'Agence'}\n")
            f.write("-" * 120 + "\n")

            for acc in self.accounts[:50]:  # Premiers 50 comptes
                f.write(f"{acc['account_number']:<15} "
                       f"{acc['customer_name'].strip():<30} "
                       f"{acc['account_type']:<6} "
                       f"{acc['current_balance']:>15,.2f} "
                       f"{acc['overdraft_limit']:>12,.2f} "
                       f"{acc['status_code']:<8} "
                       f"{acc['branch_code']}\n")

            f.write("\n" + "=" * 120 + "\n")
            f.write(f"Total: {len(self.accounts)} comptes\n")

        # Version texte des transactions
        with open(f"{OUTPUT_DIR}/transactions-readable.txt", 'w', encoding='utf-8') as f:
            f.write("=" * 130 + "\n")
            f.write("TRANSACTIONS BANCAIRES - VERSION LISIBLE\n")
            f.write("=" * 130 + "\n\n")
            f.write(f"{'ID Transaction':<18} {'Compte':<15} {'Type':<6} {'Montant':>12} {'Date':<10} {'Heure':<8} {'Référence'}\n")
            f.write("-" * 130 + "\n")

            for tran in self.transactions[:100]:  # Premières 100 transactions
                f.write(f"{tran['transaction_id']:<18} "
                       f"{tran['account_number']:<15} "
                       f"{tran['transaction_type']:<6} "
                       f"{tran['amount']:>12,.2f} "
                       f"{tran['transaction_date']:<10} "
                       f"{tran['transaction_time']:<8} "
                       f"{tran['reference'].strip()}\n")

            f.write("\n" + "=" * 130 + "\n")
            f.write(f"Total: {len(self.transactions)} transactions\n")

        print("✓ Versions texte écrites")

    def write_statistics(self):
        """Génère un fichier de statistiques"""
        with open(f"{OUTPUT_DIR}/statistics.txt", 'w', encoding='utf-8') as f:
            f.write("STATISTIQUES DES DONNÉES GÉNÉRÉES\n")
            f.write("=" * 60 + "\n\n")

            # Statistiques des comptes
            f.write("COMPTES:\n")
            f.write(f"  Total comptes: {len(self.accounts)}\n")

            type_counts = {}
            status_counts = {}
            for acc in self.accounts:
                type_counts[acc['account_type']] = type_counts.get(acc['account_type'], 0) + 1
                status_counts[acc['status_code']] = status_counts.get(acc['status_code'], 0) + 1

            f.write(f"  Par type:\n")
            for t, count in type_counts.items():
                f.write(f"    {t}: {count}\n")

            f.write(f"  Par statut:\n")
            for s, count in status_counts.items():
                status_name = {'A': 'Actif', 'F': 'Gelé', 'C': 'Fermé'}.get(s, s)
                f.write(f"    {status_name}: {count}\n")

            total_balance = sum(acc['current_balance'] for acc in self.accounts)
            f.write(f"  Solde total: {total_balance:,.2f} EUR\n")
            f.write(f"  Solde moyen: {total_balance / len(self.accounts):,.2f} EUR\n\n")

            # Statistiques des transactions
            f.write("TRANSACTIONS:\n")
            f.write(f"  Total transactions: {len(self.transactions)}\n")

            tran_type_counts = {}
            for tran in self.transactions:
                tran_type_counts[tran['transaction_type']] = tran_type_counts.get(tran['transaction_type'], 0) + 1

            f.write(f"  Par type:\n")
            for t, count in tran_type_counts.items():
                type_name = {'DB': 'Débit', 'CR': 'Crédit', 'TF': 'Transfert'}.get(t, t)
                f.write(f"    {type_name}: {count}\n")

            total_amount = sum(tran['amount'] for tran in self.transactions)
            f.write(f"  Montant total: {total_amount:,.2f} EUR\n")
            f.write(f"  Montant moyen: {total_amount / len(self.transactions):,.2f} EUR\n")

        print("✓ Statistiques écrites")

def main():
    import os

    # Créer le répertoire de sortie
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    print("=" * 70)
    print("GÉNÉRATEUR DE DONNÉES DE TEST - SYSTÈME BANCAIRE")
    print("=" * 70)
    print()

    generator = BankingDataGenerator()

    # Générer les données
    generator.generate_accounts()
    generator.generate_transactions()

    # Écrire les fichiers
    generator.write_master_account_file(f"{OUTPUT_DIR}/BANK.MASTER.ACCOUNTS")
    generator.write_transaction_file(f"{OUTPUT_DIR}/BANK.DAILY.TRANSACTIONS")
    generator.write_text_versions()
    generator.write_statistics()

    print("\n" + "=" * 70)
    print("GÉNÉRATION TERMINÉE AVEC SUCCÈS!")
    print("=" * 70)
    print(f"\nFichiers générés dans le répertoire '{OUTPUT_DIR}/':")
    print(f"  • BANK.MASTER.ACCOUNTS       - {NUM_ACCOUNTS} comptes (format binaire)")
    print(f"  • BANK.DAILY.TRANSACTIONS    - {NUM_TRANSACTIONS} transactions (format binaire)")
    print(f"  • accounts-readable.txt      - Version texte des comptes")
    print(f"  • transactions-readable.txt  - Version texte des transactions")
    print(f"  • statistics.txt             - Statistiques des données")
    print("\nUtilisation:")
    print("  Ces fichiers peuvent être utilisés avec le JCL banking-transaction.jcl")
    print("  pour tester le traitement complet des transactions bancaires.")
    print()

if __name__ == "__main__":
    main()
