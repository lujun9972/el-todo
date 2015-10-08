(setq *todo-tasks* nil)

(todo "add" "test")
(assert (equal (task-desc (car *todo-tasks*)) "test"))
(assert (equal (task-pri (car *todo-tasks*)) todo--default-task-pri))
(assert (stringp (task-create-time (car *todo-tasks*))))

(todo "show" ":desc" "t")
(assert (todo--filter :desc "t" :pri 1))
(assert (todo--filter :id (task-id (car *todo-tasks*))))
(assert (null (todo--filter :desc "t" :pri 10)))
(assert (null (todo--filter :desc "k")))

(todo "show")

(assert (todo--find-task-by-id (task-id (car *todo-tasks*))))

(todo-edit (task-id (car *todo-tasks*)) "test2")
(assert (equal "test2" (task-desc (car *todo-tasks*))))

(todo-done (task-id (car *todo-tasks*)))
(assert (task-done-time (car *todo-tasks*)))

(todo-do (task-id (car *todo-tasks*)))
(todo-pause (task-id (car *todo-tasks*)))
