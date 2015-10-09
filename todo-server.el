(defvar todo-server--server-process nil)
(defun todo-server--filter-fn (conn &rest objs)
  (let* ((cmd (car objs))
		 (cmd-fn (intern (format "todo-server-%s" cmd)))
		 (args (cdr objs)))
	(if (functionp cmd-fn)
		(apply cmd-fn conn args)
	  (message "unkown request")
	  (lispy-process-send conn (intern (format "%s-RESPONSE" cmd) nil "known request cmd")))))

(defun todo-server-start-server (&optional port)
  "start the todo server"
  (interactive)
  (unless port
	(setq port (read-number "which port: ")))
  (when todo-server--server-process
	(delete-process todo-server--server-process))
  (setq todo-server--server-process
		(make-lispy-network-process :name "todo-server"
									:server t
									:service port
									:filter #'todo-server--filter-fn)))

(defvar todo-server--users nil)

(defun todo-server--verify (user pwd)
  "verify user and password"
  (if (assoc-string user todo-server--users)
	  (member (cons user pwd) todo-server--users)
	(push (cons user pwd) todo-server--users)))

(defun todo-server--user-save-file (user)
  "return the file path used to store task data of `user'"
  (format "todo-server-%s.save" user))

(defun todo-server-PULL (conn user pwd)
  "send user's task data back to requester after verified the user "
  (if (todo-server--verify user pwd)
	  (with-temp-buffer
		(insert-file-contents (todo-server--user-save-file user))
		(lispy-process-send conn 'PULL-RESPONSE t (read-from-whole-string (buffer-string))))
	(lispy-process-send conn 'PULL-RESPONSE nil "unmatched user and password")))

(defun todo-server-PUSH (conn user pwd tasks)
  "store user's task data after verified the user "
  (if (todo-server--verify user pwd)
	  (progn
		(with-temp-file (todo-server--user-save-file user)
		  (insert (prin1-to-string tasks)))
		(lispy-process-send conn 'PUSH-RESPONSE t "task data save"))
	(lispy-process-send conn 'PUSH-RESPONSE nil "unmatched user and password")))
