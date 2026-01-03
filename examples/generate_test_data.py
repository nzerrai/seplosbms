#!/usr/bin/env python3
"""
Générateur de données de test pour CUSTPROC
Crée des fichiers de test au format COBOL et CSV
"""

import random
import csv
from datetime import datetime, timedelta
from decimal import Decimal

# Listes de noms pour générer des données aléatoires
FIRST_NAMES = [
    "Jean", "Marie", "Pierre", "Sophie", "Luc", "Emma", "Thomas", "Julie",
    "Antoine", "Claire", "Paul", "Alice", "François", "Isabelle", "Nicolas",
    "Camille", "Philippe", "Nathalie", "Laurent", "Céline", "Marc", "Sandrine",
    "David", "Valérie", "Olivier", "Sylvie", "Michel", "Catherine", "Bernard",
    "Monique", "Jacques", "Françoise", "André", "Christine", "Daniel", "Martine"
]

LAST_NAMES = [
    "Martin", "Bernard", "Dubois", "Thomas", "Robert", "Richard", "Petit",
    "Durand", "Leroy", "Moreau", "Simon", "Laurent", "Lefebvre", "Michel",
    "Garcia", "David", "Bertrand", "Roux", "Vincent", "Fournier", "Morel",
    "Girard", "Andre", "Mercier", "Dupont", "Lambert", "Bonnet", "Francois",
    "Martinez", "Legrand", "Garnier", "Faure", "Rousseau", "Blanc", "Guerin"
]


def generate_cobol_record(cust_id, cust_name, cust_amount, cust_date):
    """
    Génère un enregistrement au format COBOL position fixe

    Format:
    - CUST-ID: PIC 9(6) - 6 caractères numériques
    - CUST-NAME: PIC X(30) - 30 caractères alphanumériques
    - CUST-AMOUNT: PIC 9(7)V99 - 7+2 chiffres (représenté en texte)
    - CUST-DATE: PIC 9(8) - 8 chiffres AAAAMMJJ
    """
    # Formatter l'ID (6 caractères, padded avec des zéros)
    id_str = str(cust_id).zfill(6)

    # Formatter le nom (30 caractères, padded avec des espaces)
    name_str = cust_name[:30].ljust(30)

    # Formatter le montant (8 chiffres - 7 entiers + 2 décimales)
    # Convertir en centimes et formatter
    amount_cents = int(cust_amount * 100)
    amount_str = str(amount_cents).zfill(8)

    # Formatter la date (8 caractères AAAAMMJJ)
    date_str = cust_date.strftime("%Y%m%d")

    # Assembler l'enregistrement
    record = f"{id_str}{name_str}{amount_str}{date_str}\n"

    return record


def generate_test_data(num_records=100, output_prefix="customers"):
    """
    Génère des fichiers de test COBOL et CSV

    Args:
        num_records: Nombre d'enregistrements à générer
        output_prefix: Préfixe pour les noms de fichiers
    """
    cobol_file = f"{output_prefix}.dat"
    csv_file = f"{output_prefix}.csv"

    print(f"Génération de {num_records} enregistrements...")
    print(f"  - Fichier COBOL: {cobol_file}")
    print(f"  - Fichier CSV: {csv_file}")
    print()

    records = []
    high_value_count = 0
    total_amount = Decimal('0.00')

    # Date de départ
    start_date = datetime(2024, 1, 1)

    # Générer les enregistrements
    for i in range(1, num_records + 1):
        # ID séquentiel
        cust_id = i

        # Nom aléatoire
        first_name = random.choice(FIRST_NAMES)
        last_name = random.choice(LAST_NAMES)
        cust_name = f"{first_name} {last_name}"

        # Montant aléatoire (de 10 à 5000 euros)
        # 70% des montants sont < 1000, 30% sont > 1000
        if random.random() < 0.7:
            # Montant bas
            cust_amount = Decimal(str(round(random.uniform(10, 999), 2)))
        else:
            # Montant élevé
            cust_amount = Decimal(str(round(random.uniform(1001, 5000), 2)))
            high_value_count += 1

        total_amount += cust_amount

        # Date incrémentale (un jour par enregistrement)
        cust_date = start_date + timedelta(days=i-1)

        records.append({
            'id': cust_id,
            'name': cust_name,
            'amount': cust_amount,
            'date': cust_date
        })

    # Écrire le fichier COBOL
    with open(cobol_file, 'w', encoding='utf-8') as f:
        for rec in records:
            cobol_record = generate_cobol_record(
                rec['id'],
                rec['name'],
                rec['amount'],
                rec['date']
            )
            f.write(cobol_record)

    print(f"✓ Fichier COBOL créé: {cobol_file}")

    # Écrire le fichier CSV
    with open(csv_file, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(['CUST_ID', 'CUST_NAME', 'CUST_AMOUNT', 'CUST_DATE'])

        for rec in records:
            writer.writerow([
                str(rec['id']).zfill(6),
                rec['name'],
                f"{rec['amount']:.2f}",
                rec['date'].strftime("%Y%m%d")
            ])

    print(f"✓ Fichier CSV créé: {csv_file}")
    print()

    # Statistiques
    avg_amount = total_amount / num_records

    print("📊 Statistiques:")
    print(f"  - Total d'enregistrements: {num_records}")
    print(f"  - Montants > 1000€: {high_value_count} ({high_value_count*100/num_records:.1f}%)")
    print(f"  - Montant total: {total_amount:.2f}€")
    print(f"  - Montant moyen: {avg_amount:.2f}€")
    print()

    # Afficher les 5 premiers enregistrements
    print("📝 Aperçu des 5 premiers enregistrements:")
    print()
    print("ID     | Nom                            | Montant    | Date     | HIGH?")
    print("-------|--------------------------------|------------|----------|-------")

    for rec in records[:5]:
        high_marker = "✓" if rec['amount'] > 1000 else ""
        print(f"{rec['id']:06d} | {rec['name']:30s} | {rec['amount']:>9.2f}€ | {rec['date'].strftime('%d/%m/%y')} | {high_marker}")

    print()
    print("✅ Génération terminée!")
    print()
    print("🔍 Vérifiez les fichiers générés:")
    print(f"   cat {cobol_file}")
    print(f"   cat {csv_file}")


def main():
    """Point d'entrée principal"""
    import argparse

    parser = argparse.ArgumentParser(
        description='Générateur de données de test pour CUSTPROC'
    )
    parser.add_argument(
        '-n', '--num-records',
        type=int,
        default=100,
        help='Nombre d\'enregistrements à générer (défaut: 100)'
    )
    parser.add_argument(
        '-o', '--output',
        type=str,
        default='customers_generated',
        help='Préfixe pour les fichiers de sortie (défaut: customers_generated)'
    )
    parser.add_argument(
        '--seed',
        type=int,
        help='Seed pour la génération aléatoire (pour reproductibilité)'
    )

    args = parser.parse_args()

    # Définir le seed si spécifié
    if args.seed:
        random.seed(args.seed)
        print(f"🎲 Seed aléatoire: {args.seed}")
        print()

    # Générer les données
    generate_test_data(args.num_records, args.output)


if __name__ == '__main__':
    main()
