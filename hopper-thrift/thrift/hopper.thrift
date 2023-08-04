typedef binary TaskId

struct RequestNextTaskRequest {
    1: i32 noop;
}

struct RequestNextTaskResponse {
    1: optional TaskId task_id;
    2: optional binary task;
    3: optional i32 timeout_in_seconds;
}

union TaskResult {
    1: binary task_result;
    2: string error_message;
}

struct HeartbeatRequest {
    1: optional TaskId task_id;
    2: optional TaskResult task_result;
}

service Scheduler {
    RequestNextTaskResponse requestNextTask(1: RequestNextTaskRequest requestNextTaskRequest),

    void heartbeat(1: HeartbeatRequest heartbeat)
}
