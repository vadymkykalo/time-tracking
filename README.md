# time-tracking
Employee Work Time Tracking Microservice

## Description

This microservice is designed for tracking employee work time using NFC cards. Employees register their arrival at work and departure by placing their card on a reader. Administrators can view history, manage work schedules, and add exceptions (vacations, permitted late arrivals, etc.).

## Technical Stack

- Erlang
- RabbitMQ (for RPC server)
- PostgreSQL (for data storage)
- Docker and Docker Compose (for deployment)

## API Methods

### Card Management

- `/card/touch` - Register card touch (check-in/check-out)
  - Required fields: `card_uid`
  - Response example: `{card_uid: string, user_id: number}`

- `/card/assign` - Assign card to employee
  - Required fields: `user_id`, `card_uid`
  - Response example: `{card_uid: string, user_id: number}`

- `/card/delete` - Delete card
  - Required fields: `card_uid`
  - Response example: `{card_uid: string, user_id: number}`

- `/card/list_by_user` - Get list of employee's cards
  - Required fields: `user_id`
  - Response example: `{user_id: Array<card_uid>}`

- `/card/delete_all_by_user` - Delete all employee cards
  - Required fields: `user_id`
  - Response example: `{user_id: Array<card_uid>}`

### Work Time Management

- `/work_time/set` - Set employee work schedule
  - Required fields: `user_id`, `start_time`, `end_time`, `days`, `free_schedule`
  - Request example: `{user_id: 1, start_time: "09:00:00", end_time: "18:00:00", days: [1,2,3,4,5], free_schedule: false}`

- `/work_time/get` - Get employee work schedule
  - Required fields: `user_id`

- `/work_time/add_exclusion` - Add exception to schedule
  - Required fields: `user_id`, `type_exclusion`, `start_datetime`, `end_datetime`
  - `type_exclusion` - exception type: late_arrival, early_departure, full_day_off
  - Request example: `{user_id: 1, type_exclusion: "late_arrival", start_datetime: "2025-07-15 10:00:00", end_datetime: "2025-07-15 18:00:00"}`

- `/work_time/get_exclusion` - Get schedule exceptions
  - Required fields: `user_id`

- `/work_time/history_by_user` - Get employee work history
  - Required fields: `user_id`

- `/work_time/history` - Get history for all employees
  - Required fields: `limit`

- `/work_time/statistics_by_user` - Get employee work statistics
  - Required fields: `user_id`
  - Optional fields: `period` (values: week, month, year, all; default - month)

- `/work_time/statistics` - Get general statistics
  - Required fields: `limit`

## Running

### Locally with Docker Compose

```bash
docker-compose up -d
```

### Running Tests

```bash
docker-compose -f docker-compose.test.yml up
```

## Configuration

Configuration files are located at:
- `config/sys.config` - main application settings
- `config/vm.args` - Erlang virtual machine settings
