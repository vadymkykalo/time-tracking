-- Database initialization for the time tracking microservice

-- Users table
CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- NFC cards table
CREATE TABLE IF NOT EXISTS cards (
    uid VARCHAR(255) PRIMARY KEY,
    user_id INTEGER REFERENCES users(id),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Work schedules table
CREATE TABLE IF NOT EXISTS work_schedules (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id),
    start_time TIME NOT NULL,
    end_time TIME NOT NULL,
    days INTEGER[] NOT NULL,
    free_schedule BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Schedule exceptions table
CREATE TABLE IF NOT EXISTS schedule_exclusions (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id),
    type_exclusion VARCHAR(50) NOT NULL,
    start_datetime TIMESTAMP NOT NULL,
    end_datetime TIMESTAMP NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Time logs table
CREATE TABLE IF NOT EXISTS time_logs (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id),
    card_uid VARCHAR(255) REFERENCES cards(uid),
    log_type VARCHAR(10) NOT NULL,
    log_time TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for query optimization
CREATE INDEX IF NOT EXISTS idx_cards_user_id ON cards(user_id);
CREATE INDEX IF NOT EXISTS idx_work_schedules_user_id ON work_schedules(user_id);
CREATE INDEX IF NOT EXISTS idx_schedule_exclusions_user_id ON schedule_exclusions(user_id);
CREATE INDEX IF NOT EXISTS idx_time_logs_user_id ON time_logs(user_id);
CREATE INDEX IF NOT EXISTS idx_time_logs_log_time ON time_logs(log_time);

-- Test data
INSERT INTO users (name) VALUES ('Ivan Ivanov') ON CONFLICT DO NOTHING;
INSERT INTO users (name) VALUES ('Maria Petrova') ON CONFLICT DO NOTHING;

-- Test cards
INSERT INTO cards (uid, user_id) VALUES ('card123', 1) ON CONFLICT DO NOTHING;
INSERT INTO cards (uid, user_id) VALUES ('card456', 2) ON CONFLICT DO NOTHING;

-- Test work schedules
INSERT INTO work_schedules (user_id, start_time, end_time, days, free_schedule) 
VALUES (1, '09:00:00', '18:00:00', '{1,2,3,4,5}', false) ON CONFLICT DO NOTHING;
INSERT INTO work_schedules (user_id, start_time, end_time, days, free_schedule) 
VALUES (2, '10:00:00', '19:00:00', '{1,2,3,4,5}', false) ON CONFLICT DO NOTHING;
