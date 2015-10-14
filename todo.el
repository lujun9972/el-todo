#!emacs --script
(require 'cl)
(require 'subr-x)

(defun make-lispy-network-process (&rest args)
  "类似`make-network-process'但使用lisp object作为传输对象

filter function的函数签名应该为(process &rest objs) "
  (lexical-let* ((ori-filter-fn (or  (plist-get args :filter)
									 #'lispy-process-default-filter))
				 (store-msg-property (gensym)))
	(plist-put args :filter
			   (lambda (process msg)
				 (let ((content (process-get process store-msg-property))
					   result obj)
				   (setq content (concat content msg))
				   (while (setq result (ignore-errors (read-from-string content)))
					 (setq content (substring content (cdr result)))
					 (setq obj (car result))
					 (apply ori-filter-fn process obj))
				   (process-put process store-msg-property content))))
	(apply #'make-network-process args)))

(defun lispy-process-send (process &rest objs)
  "类似`process-send-string' 但发送的是lisp object"
  (process-send-string process (prin1-to-string objs)))

(defun lispy-process-send-wait (process wait-flag &rest objs)
  "类似`lispy-process-send' 但会等待回应
其中会设置process的'WAIT属性为`wait-flag. 并等待回应函数将'WAIT属性清为nil"
  (process-put process 'WAIT wait-flag)
  (process-send-string process (prin1-to-string objs))
  (while (process-get process 'WAIT)
	(accept-process-output process 0.05)))

(defgroup el-todo nil
  "a todo command line tool used in eshell")

(defvar *todo-tasks* nil
  "任务列表")

(defcustom todo--default-task-pri 5
  "任务默认优先级")

(cl-defun todo--gen-task-id ()
  "generate a new task-id"
  (gensym))

(defstruct task
  (id (todo--gen-task-id))
  (desc (read-string "请输入任务描述: "))
  (pri todo--default-task-pri)
  (tags nil)
  (create-time (current-time-string))
  (schedule-time nil)
  (deadline-time nil)
  (done-time nil)
  (doing-periods nil))

(cl-defun todo (cmd &rest args)
  "The main function"
  (apply (intern (format "todo-%s" (downcase cmd)))
		 (mapcar (lambda (arg)
				   (if (stringp arg)
					   (cond ((string-prefix-p ":" arg)
							  (intern arg))
							 ((string-match-p "^[[:digit:]]+$" arg)
							  (string-to-number arg))
							 (t
							  arg))
					 arg)) args)))


;; todo add task-description
(cl-defun todo-add (description &key pri tags sd dl)
  "add a new task"
  (push (make-task :desc description
				   :pri (or pri todo--default-task-pri)
				   :tags tags
				   :schedule-time sd
				   :deadline-time dl) *todo-tasks*))

;; todo show
(cl-defun todo--find-undone-tasks (tasks)
  "find only unfinished tasks"
  (remove-if #'task-done-time tasks))

(cl-defun todo--filter (&key id desc pri tag done)
  "filter and list tasks
`:done'表示同时显示已完成的任务
`:desc'参数表示只显示符合所指定正则的任务"
  (let ((tasks *todo-tasks*))
	(unless done
	  (setq tasks (todo--find-undone-tasks tasks)))
	(when id
	  (setq tasks (remove-if-not (lambda (task)
								   (string-equal id (task-id task)))
								 tasks)))
	(when desc
	  (setq tasks (remove-if-not (lambda (task)
								   (string-match-p desc (task-desc task)))
								 tasks)))
	(when pri
	  (setq tasks (remove-if-not (lambda (task)
								   (<= pri (task-pri task)))
								 tasks)))
	(when tag
	  (setq tasks (remove-if-not (lambda (task)
								   (member tag (task-tags task)))
								 tasks)))
	tasks))

(cl-defun todo--show-task (task)
  (let ((task-info (format "%s:%s\t" (task-id task) (task-desc task)))
		(pri-info (and (task-pri task)
					   (format "优先级:%s" (task-pri task))))
		(done-info (and (task-done-time task)
						(format  "完成时间:%s" (task-done-time task))))
		(sd-info (and (task-schedule-time task)
					  (format "规划时间:%s" (task-schedule-time task))))
		(dl-info (and (task-deadline-time task)
					  (format "截止时间:%s" (task-deadline-time task)))))
	(dolist (element-info (list pri-info done-info sd-info dl-info))
	  (when element-info
		(setq task-info (concat task-info " " element-info))))
	(message task-info)))

(cl-defun todo--show-tasks (tasks)
  (string-join (mapcar #'todo--show-task tasks) "\n"))

(cl-defun todo-show (&key desc pri tag (done nil done-p))
  "filter and list tasks"
  (let ((tasks (todo--filter :desc desc :pri pri :tag tag :done done-p)))
	(todo--show-tasks tasks)))

;; todo edit task-id task-description
(cl-defun todo--find-task-by-id (id)
  "find task by `id'. If no task found,throw an error"
  (let ((task (car (todo--filter :id id))))
	(unless task
	  (error "not found No.%s task" id))
	task))

(defmacro todo--with-task (id &rest body)
  (declare (indent 2) (debug t))
  `(let ((THE-TASK (todo--find-task-by-id ,id)))
	,@body))

(cl-defun todo-edit (id desc)
  "edit task's description"
  (todo--with-task id
	(setf (task-desc THE-TASK) desc)))

;; todo pri task-id new-pri
(cl-defun todo-pri (task-id pri)
  "set/change task's priority"
  (todo--with-task id
	  (setf (task-pri THE-TASK) pri)))

;; todo sd task-id 
(cl-defun todo-sd (task-id)
  "set/change task's schedule date"
  (todo--with-task id
	  (setf (task-schedule-time THE-TASK) (org-read-date))))

;; todo dl task-id 
(cl-defun todo-dl (task-id)
  "set/change task's schedule date"
  (todo--with-task id
	  (setf (task-deadline-time THE-TASK) (org-read-date))))

;; todo done task-id
(cl-defun todo-done (id)
  "mark a task done,and remember the finish time"
  (todo--with-task id 
	  (setf (task-done-time THE-TASK) (current-time-string))))

;; todo do task-id
(cl-defun todo-do (id)
  "mark doing a task"
  (todo--with-task id
	  (push (list (current-time-string)) (task-doing-periods THE-TASK))))

;; todo pause task-id
(cl-defun todo-pause (id)
  "mark take a break of doing a task"
  (todo--with-task id
	  (append (car (task-doing-periods THE-TASK)) (current-time-string))))

;; todo save
(defcustom todo-save-file (concat default-directory "todo-file.save")
  "the file used to save tasks")

(cl-defun todo-save (&optional (save-file todo-save-file))
  "save todo tasks"
  (with-temp-file save-file
	(insert (prin1-to-string *todo-tasks*))))

;; todo load
(cl-defun todo-load (&optional (save-file todo-save-file))
  "load todo tasks"
  (with-temp-buffer
	(insert-file-contents save-file)
	(setq *todo-tasks* (read-from-whole-string (buffer-string)))))

;; todo pull server user pwd
(defvar todo-client--process nil)

(defcustom todo-server "127.0.0.1"
  "the address of todo server")

(defcustom todo-port 7000
  "the port of todo server")

(cl-defun todo--connect-to-server (server port)
  "make a process connect to the todo server"
  (unless (and todo-client--process
			   (process-live-p todo-client--process))
	(setq todo-client--process (make-lispy-network-process :name "todo-client"
							  :host server
							  :service port
							  :filter (lambda (proc &rest objs)
										(let* ((cmd (car objs))
											   (cmd-fn (intern (format "todo--%s" cmd)))
											   (args (cdr objs)))
										  (when (eq (process-get proc 'WAIT) cmd)
											(process-put proc 'WAIT nil))
										  (apply cmd-fn proc args)))))))

(cl-defun todo-pull (&optional (server todo-server) (port todo-port) user pwd)
  (let ((user (or user
				  (read-string "please input the user name: " user-login-name)))
		(pwd (md5 (or pwd
					  (read-passwd "please input the password: ")))))
	(todo--connect-to-server server port)
	(lispy-process-send-wait todo-client--process 'PULL-RESPONSE
							 'PULL user pwd)))
(cl-defun todo--PULL-RESPONSE (conn status tasks-or-error)
  ""
  (if status
	  (setq *todo-tasks* tasks-or-error)
	(error tasks-or-error)))

;; todo push server
(cl-defun todo-push (&optional (server todo-server) (port todo-port) user pwd)
  (let ((user (or user
				  (read-string "please input the user name: " user-login-name)))
		(pwd (md5 (or pwd
					  (read-passwd "please input the password: ")))))
	(todo--connect-to-server server port)
	(lispy-process-send-wait todo-client--process 'PUSH-RESPONSE
							 'PUSH user pwd *todo-tasks*)))
(cl-defun todo--PUSH-RESPONSE (conn status &optional tasks-or-error)
  ""
  (unless status
	(error tasks-or-error)))

;; 以下操作是为了兼容#!emacs --script方式
(when (member "-scriptload" command-line-args)
  (todo "pull")
  (catch EXIT
	(let (command)
	  (while t
		(setq command (read-string ": "))
		(if (string= (downcase command) "exit")
			(throw EXIT t)
		  (apply #'todo (split-string command))))))
  (todo "push"))
