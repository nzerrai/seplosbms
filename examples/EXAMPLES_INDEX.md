# 📚 Index des Exemples COBOL/JCL

Ce répertoire contient des exemples COBOL et JCL de complexité croissante.

## 🎯 Exemples disponibles

| Exemple | Complexité | Fichiers | Lignes |
|---------|-----------|----------|--------|
| **Customer Simple** | ⭐ Débutant | `customer.cob` + JCL | 100 COBOL |
| **Banking Transaction** | ⭐⭐⭐⭐⭐ Avancé | `banking-transaction.cob` + JCL | 450+ COBOL, 200+ JCL |

## 🚀 Utilisation rapide

```bash
# 1. Générer les données de test
cd examples
./generate_all_test_data.sh

# 2. Lancer le traducteur
java -jar target/cobol-translator.jar

# 3. Uploader les fichiers via http://localhost:9090/conversion
```

Voir [GENERATORS_README.md](GENERATORS_README.md) pour plus de détails.

---

**Version** : 1.0.0 | **Date** : Janvier 2026
