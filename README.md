# README
---
This webapp offers digital management of a Pilates course using SQLite. It is heavily based on the requirements of [PilatesCare](https://pilatescare.ch/) in Switzerland, but can be adjusted to your needs.

## Features

- Add therapists and their credentials
- Add as many courses and course dates as you like
- Add course members and track their course memberships as well as course attendance
- Keep track of course subscriptions (abos) and when a new subscription is due with the abo dashboard 
- Automatically generate course certificates 

## Usage

1. Clone the repository to your local machine
2. In the [signatures](internal/signatures/) folder, you will find two files. Take [unsigned_line.png](internal/signatures/unsigned_line.png), add your signature to it and upload it when adding a therapist to get your signature on the automatically generated certificates.
3. Inside the [webapp](internal/webapp/) folder, you will find [global.R](internal/webapp/global.R). Run this to start the webapp. The SQLite database will be created automatically on your local machine
4. Fill the database with information.
5. Generate certificates automatically. They will be placed in their own folder named Zertifikate.

## License
                        GNU GENERAL PUBLIC LICENSE
                          Version 3, 29 June 2007
     
     Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>
     Everyone is permitted to copy and distribute verbatim copies
     of this license document, but changing it is not allowed.
