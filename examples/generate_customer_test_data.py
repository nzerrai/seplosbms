#!/usr/bin/env python3
"""
Générateur de données de test pour l'exemple customer simple
"""

import random
from datetime import datetime, timedelta

# Configuration
NUM_CUSTOMERS = 100
OUTPUT_DIR = "test-data"

FIRST_NAMES = [
    "Jean", "Marie", "Pierre", "Sophie", "Luc", "Anne", "Marc", "Julie",
    "Francois", "Claire", "Michel", "Isabelle", "Philippe", "Catherine",
    "Bernard", "Sylvie", "Andre", "Monique", "Jacques", "Nicole"
]

LAST_NAMES = [
    "Martin", "Bernard", "Dubois", "Thomas", "Robert", "Richard",
    "Petit", "Durand", "Leroy", "Moreau", "Simon", "Laurent",
    "Lefebvre", "Michel", "Garcia", "David", "Bertrand", "Roux"
]

CITIES = [
    "Paris", "Lyon", "Marseille", "Toulouse", "Nice", "Nantes",
    "Strasbourg", "Montpellier", "Bordeaux", "Lille", "Rennes", "Reims"
]

def generate_customer_name():
    """Génère un nom de client"""
    first = random.choice(FIRST_NAMES)
    last = random.choice(LAST_NAMES)
    return f"{first} {last}".ljust(30)[:30]

def generate_address():
    """Génère une adresse"""
    num = random.randint(1, 999)
    street = random.choice(["Rue", "Avenue", "Boulevard", "Place"])
    name = random.choice(["Victor Hugo", "de la République", "du Général de Gaulle", "Voltaire"])
    return f"{num} {street} {name}".ljust(50)[:50]

def generate_city():
    """Génère une ville"""
    return random.choice(CITIES).ljust(30)[:30]

def generate_postal_code():
    """Génère un code postal français"""
    return f"{random.randint(1000, 99999):05d}"

def generate_phone():
    """Génère un numéro de téléphone"""
    return f"0{random.randint(1, 9)}{random.randint(10000000, 99999999):08d}"

def main():
    import os

    os.makedirs(OUTPUT_DIR, exist_ok=True)

    print("=" * 70)
    print("GÉNÉRATEUR DE DONNÉES DE TEST - CLIENTS")
    print("=" * 70)
    print(f"\nGénération de {NUM_CUSTOMERS} clients...")

    # Format: Record de 80 bytes selon LRECL=80 du JCL
    with open(f"{OUTPUT_DIR}/CUSTOMER.INPUT.DATA", 'wb') as f:
        for i in range(1, NUM_CUSTOMERS + 1):
            # Format simple: ID(6) + Name(30) + Balance(10) + Filler(34)
            customer_id = f"{i:06d}"
            name = generate_customer_name()
            balance = f"{random.uniform(0, 99999.99):010.2f}"
            filler = " " * 34

            record = f"{customer_id}{name}{balance}{filler}".encode('ascii')

            # Assurer 80 bytes
            if len(record) < 80:
                record += b' ' * (80 - len(record))
            elif len(record) > 80:
                record = record[:80]

            f.write(record)

    print(f"✓ {NUM_CUSTOMERS} clients générés")

    # Version lisible
    with open(f"{OUTPUT_DIR}/customers-readable.txt", 'w', encoding='utf-8') as f:
        f.write("FICHIER CLIENT - VERSION LISIBLE\n")
        f.write("=" * 70 + "\n\n")
        f.write(f"{'ID':<8} {'Nom':<32} {'Solde':>12}\n")
        f.write("-" * 70 + "\n")

        with open(f"{OUTPUT_DIR}/CUSTOMER.INPUT.DATA", 'rb') as input_file:
            for line in input_file:
                customer_id = line[0:6].decode('ascii').strip()
                name = line[6:36].decode('ascii').strip()
                balance = line[36:46].decode('ascii').strip()

                f.write(f"{customer_id:<8} {name:<32} {float(balance):>12,.2f}\n")

        f.write("\n" + "=" * 70 + "\n")
        f.write(f"Total: {NUM_CUSTOMERS} clients\n")

    print("\n" + "=" * 70)
    print("GÉNÉRATION TERMINÉE!")
    print("=" * 70)
    print(f"\nFichiers générés dans '{OUTPUT_DIR}/':")
    print(f"  • CUSTOMER.INPUT.DATA     - {NUM_CUSTOMERS} clients (80 bytes/record)")
    print(f"  • customers-readable.txt  - Version texte lisible")
    print("\nUtilisation:")
    print("  Ces fichiers peuvent être utilisés avec le JCL customer-batch.jcl")
    print()

if __name__ == "__main__":
    main()
