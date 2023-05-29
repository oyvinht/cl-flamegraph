(defpackage #:flamegraph
  (:use #:cl)
  (:import-from #:alexandria)
  #+sbcl
  (:import-from #:sb-sprof)
  (:nicknames #:flamegraph/core)
  (:export
   #:save-flame-graph))
(in-package flamegraph)


(defparameter *frame-where-profiling-was-started* nil)


(defclass node ()
  ((func :initarg :func
         :initform nil
         :type (or string
                   #+sbcl
                   sb-di::compiled-debug-fun
                   null)
         :accessor get-func)
   (counter :initform 0
            :type fixnum
            :initarg :counter
            :accessor get-counter)
   (calls :initform nil
          :type list
          :initarg :calls
          :documentation "A list of other nodes, called by current one"
          :accessor get-calls)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A :calls ~A"
            (or (get-func node)
                "<root>")
            (get-counter node))))

(defun search-or-add-child (node func)
  ;; Not all frames contain an info for some reason.
  ;; We only want to show  meaningfull nodes
  (when func
    (let* ((children (get-calls node))
           (child (find func children
                        :test #'equal
                        :key #'get-func)))
      (unless child
        (setf child (make-instance 'node :func func))
        (push child (get-calls node)))
      child)))

(defgeneric get-name (obj))

(defmethod get-name ((obj node))
  (get-name (get-func obj)))

(defmethod get-name ((obj string))
  obj)

#+sbcl
(defmethod get-name ((obj sb-di::compiled-debug-fun))
  (format t "~%obj: ~a~%name: ~a%" obj (get-name (slot-value obj 'SB-DI::COMPILER-DEBUG-FUN)))
  (get-name (slot-value obj 'SB-DI::COMPILER-DEBUG-FUN)))

#+allegro
(defmethod get-name ((obj vector))
  obj)

#+sbcl
(defmethod get-name ((obj SB-C::COMPILED-DEBUG-FUN))
  (get-name (slot-value obj 'SB-C::NAME)))

#+allegro
(defmethod get-name ((obj function))
  (slot-value obj 'symdef))

(defmethod get-name ((obj cons))
  (let ((*print-pretty* nil))
    (format nil "~S" obj)))

(defmethod get-name ((obj symbol))
  (symbol-name obj))

#+sbcl
(defmethod get-name ((obj sb-kernel:code-component))
  "Some binary code")

#+allegro
(defmethod get-name ((obj t))
  obj)

#+sbcl
(defun aggregate-raw-data ()
  ;; We need to actually run a report once to make the call graph
  ;; available to map.
  (sb-sprof:report :stream (make-broadcast-stream)))

#+sbcl
(defun make-graph ()
  (aggregate-raw-data)
  (let ((root (make-instance 'node)))
    (sb-sprof:map-traces
     (lambda (thread trace)
       (declare (ignorable thread))
       (let ((current-node root))
         (sb-sprof::map-trace-pc-locs
          (lambda (info pc-or-offset)
            (declare (ignorable pc-or-offset))
            (let ((node (search-or-add-child current-node
                                             info)))
              (when node
                (incf (get-counter node))
                (setf current-node
                      node))))
          trace)))
     sb-sprof::*samples*)
    root))


(defun remove-nodes-up-to-frame (nodes frame)
  (let ((func (slot-value
               frame
               #+sbcl
               'sb-di::debug-fun
               #+allegro
               nil)))
    (loop for rest on nodes
          for node = (car rest)
          when (eql (get-func node)
                    func)
            do (return (cdr rest)))))


(defun print-graph (root &key (stream t) (max-depth most-positive-fixnum))
  (let* ((roots (get-calls root)))
    (labels ((print-path (path count)
               (let* ((nodes (reverse path))
                      (rest-nodes #+sbcl
                                  (remove-nodes-up-to-frame nodes
                                                            *frame-where-profiling-was-started*)
                                  #+allegro
                                  nodes)
                      (names (mapcar #'get-name rest-nodes)))
                 (when names
                   (format stream "~{~A~^;~} ~A~%"
                           names
                           count))))
             (print-node (node &optional path (depth 0))
               (when (< depth max-depth)
                 (let* ((count (get-counter node))
                        (path (list* node path))
                        (children (get-calls node)))
                   (when (> count 0)
                     (print-path path count))
                   (loop for child in children
                         do (print-node child path (1+ depth)))))))
      (mapcar #'print-node
              roots)
      (values))))

#+sbcl
(defmacro save-flame-graph ((filename &rest #+sbcl sb-sprof-opts #+allegro prof-opts) &body body)
  (alexandria:with-gensyms (result-var)
    `(let ((*frame-where-profiling-was-started*
             #+sbcl (sb-di:top-frame)
             #+allegro nil)
           (,result-var nil))
       (with-simple-restart (abort "Stop profiling and save graph")

         `(#+sbcl
           ,@(sb-sprof:with-profiling (,@sb-sprof-opts))
           #+allegro
           ,@(prof:with-profiling (,@prof-opts))
           ,(setf ,result-var
                  (multiple-value-list
                   (progn ,@body)))))
       (alexandria:with-output-to-file (s ,filename :if-exists :supersede)
         (print-graph (make-graph)
                      :stream s))
       (values-list ,result-var))))


(defun make-graph (tree &optional (count 0))
  (make-instance 'flamegraph::node
                 :func (slot-value tree 'prof::entry)
                 :counter count
                 :calls (mapcar #'(lambda (c)
                                    (make-graph c (1+ count)))
                                (slot-value tree 'prof::children))))

(defun build (saved-profile)
  (with-slots (prof::actual) saved-profile
    (with-slots (prof::root-node) prof::actual
      (make-graph prof::root-node))))
