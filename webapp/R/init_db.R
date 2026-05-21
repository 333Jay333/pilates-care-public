init_db <- function(db) {
  # create therapists table
  dbExecute(
    db,
    "CREATE TABLE IF NOT EXISTS therapists (
      user_id INTEGER PRIMARY KEY AUTOINCREMENT,
      vorname TEXT NOT NULL,
      name TEXT NOT NULL,
      praxis TEXT NOT NULL,
      adresse TEXT NOT NULL,
      plz TEXT NOT NULL,
      tel TEXT NOT NULL,
      mail TEXT NOT NULL,
      zsr TEXT NOT NULL,
      knr TEXT NOT NULL,
      emfit TEXT NOT NULL,
      pilat_nr INTEGER NOT NULL
    )"
  )
  
  # create members table
  dbExecute(
    db,
    "CREATE TABLE IF NOT EXISTS members (
      user_id INTEGER PRIMARY KEY AUTOINCREMENT,
      kk TEXT NOT NULL,
      zv TEXT NOT NULL,
      vnr TEXT NOT NULL,
      vorname TEXT NOT NULL,
      name TEXT NOT NULL,
      adresse TEXT NOT NULL,
      plz TEXT NOT NULL,
      mail TEXT NOT NULL,
      status TEXT DEFAULT 'active'
    )"
  )
  
  # create courses table
  dbExecute(
    db,
    "CREATE TABLE IF NOT EXISTS courses (
      course_id INTEGER PRIMARY KEY AUTOINCREMENT,
      kursname TEXT NOT NULL,
      location TEXT NOT NULL
    )"
  )
  
  # create course_dates table
  dbExecute(
    db,
    "CREATE TABLE IF NOT EXISTS course_dates (
      course_date_id INTEGER PRIMARY KEY AUTOINCREMENT,
      course_id INTEGER NOT NULL,
      course_date DATE NOT NULL,
      FOREIGN KEY (course_id) REFERENCES courses(course_id) ON DELETE CASCADE
    )"
  )
  
  # create course membership table
  dbExecute(
    db,
    "CREATE TABLE IF NOT EXISTS course_memberships (
      user_id INTEGER NOT NULL,
      course_id INTEGER NOT NULL,
      joined_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      left_at TIMESTAMP,
      PRIMARY KEY (user_id, course_id),
      FOREIGN KEY (user_id) REFERENCES members(user_id) ON DELETE CASCADE,
      FOREIGN KEY (course_id) REFERENCES courses(course_id) ON DELETE CASCADE
    )"
  )
  
  # create abos table
  dbExecute(
    db,
    "CREATE TABLE IF NOT EXISTS abos (
      abo_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      abo_type INTEGER NOT NULL,
      abo_start DATE NOT NULL,
      abo_end DATE DEFAULT ' ',
      abo_status TEXT DEFAULT 'active',
      FOREIGN KEY (user_id) REFERENCES members(user_id) ON DELETE CASCADE
    )"
  )
  
  # create abo_prices table
  dbExecute(
    db,
    "CREATE TABLE IF NOT EXISTS abo_prices (
      course_id INTEGER NOT NULL,
      abo_type INTEGER NOT NULL,
      abo_price INTEGER NOT NULL DEFAULT 300,
      PRIMARY KEY (course_id, abo_type),
      FOREIGN KEY (course_id) REFERENCES courses(course_id) ON DELETE CASCADE
    )"
  )
  
  # create attendance table
  dbExecute(
    db,
    "CREATE TABLE IF NOT EXISTS attendance (
      course_date_id INTEGER NOT NULL,
      user_id INTEGER NOT NULL,
      status TEXT DEFAULT 'anwesend',
      PRIMARY KEY (course_date_id, user_id),
      FOREIGN KEY (course_date_id) REFERENCES course_dates(course_date_id) ON DELETE CASCADE,
      FOREIGN KEY (user_id) REFERENCES members(user_id) ON DELETE CASCADE
    )"
  )
}