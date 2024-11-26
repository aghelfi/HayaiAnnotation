
```
# Database Setup for the `db` Directory

This directory contains the necessary files for database setup. Follow the steps below to download and verify the required database files.

---

## Step 1: Download the Database

You can use either `wget` or `curl` to download the taxonomy database. Choose one of the following commands:

### Option 1: Using `wget`

```
wget -c -t 10 --retry-connrefused --waitretry=10 "https://drive.google.com/drive/folders/1C33bsP8HvsRlhsOJB4YWHWkttpDwMqNw?usp=sharing"
```

### Option 2: Using `curl`

```
curl -C - --retry 10 --retry-delay 10 -O "https://drive.google.com/drive/folders/1C33bsP8HvsRlhsOJB4YWHWkttpDwMqNw?usp=sharing"
```

---

## Step 2: Verify the Download

After downloading, verify the integrity of the file using `md5sum` and the provided checksum file.

```
md5sum -c zen.dmnd.md5
```

This command will compare the checksum of the downloaded file against the provided checksum in `zen.dmnd.md5`. If the file is intact, you will see a message indicating success.

---

### Notes:
- If the checksum verification fails, re-download the file to avoid data corruption issues.
``` 


