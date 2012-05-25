(module neo4j
  (connect
   create-node
   get-node
   delete-node
   get-relationship
   create-relationship
   delete-relationship
   get-all-relationship-properties
   set-all-relationship-properties
   get-relationship-property
   set-relationship-property
   get-all-relationships
   get-incoming-relationships
   get-outgoing-relationships
   get-all-typed-relationships
   get-incoming-typed-relationships
   get-outgoing-typed-relationships
   get-relationship-types
   get-node-property
   set-all-node-properties
   get-all-node-properties
   delete-all-node-properties
   delete-node-property
   delete-relationship-property)

  (import chicken scheme)

  (use alist-lib
       debug
       http-client
       intarweb
       json
       medea
       uri-common)

 (define-record service-root
   cypher
   relationship-index
   node
   relationship-types
   neo4j-version
   batch
   extensions-info
   node-index
   reference-node
   extensions)

 (define-record-printer service-root
   (lambda (service-root output)
     (pp (record->vector service-root) output)))

 (define-record node
   outgoing-relationships
   data
   traverse
   all-typed-relationships
   property
   self
   properties
   outgoing-typed-relationships
   incoming-relationships
   extensions
   create-relationship
   paged-traverse
   all-relationships
   incoming-typed-relationships)

 (define-record-printer node
   (lambda (node output)
     (pp (record->vector node) output)))

 (define-record exception
   message
   exception
   stacktrace)

 (define-record-printer exception
   (lambda (exception output)
     (pp (record->vector exception) output)))

 (define-record relationship
   start
   data
   self
   property
   properties
   type
   extensions
   end)

 (define-record-printer relationship
   (lambda (exception output)
     (pp (record->vector exception) output)))

 (define (connect url)
   (let ((service-root (with-input-from-request
                        url
                        #f
                        read-json)))
     (debug service-root)
     (apply make-service-root (alist-values service-root))))

 (define create-node
   (case-lambda
    ((service-root)
     (create-node service-root '()))
    ((service-root properties)
     (let ((node (with-input-from-request
                  (service-root-node service-root)
                  (json->string properties)
                  read-json)))
       (apply make-node (alist-values node))))))

;;; Does it make more sense to key off of the id? Or should we have an
;;; node-id->self?
 (define (get-node node-self)
   (let ((node (with-input-from-request
                node-self
                #f
                read-json)))
     (apply make-node (alist-values node))))

 ;; Why not `delete-node!'; and, if so, why not `create-node!'?
 ;; `create-node!' is a little awkward, since it would return a value.
 ;; `delete-node!', on the other hand, would not.
 (define (delete-node node)
   (with-input-from-request
    (make-request method: 'DELETE
                  uri: (uri-reference (node-self node)))
    #f
    values))

 ;; <https://github.com/michaelklishin/neocons/blob/master/src/clojure/clojurewerkz/neocons/rest/relationships.clj#L82>,
 ;; as an example, fetches by integer id.
 (define (get-relationship relationship-self)
   (let ((relationship
          (with-input-from-request
           relationship-self
           #f
           read-json)))
     (apply make-relationship (alist-values relationship))))

 (define create-relationship
   (case-lambda
    ((from to type)
     (create-relationship from to type '()))
    ((from to type data)
     (let ((relationship
            (with-input-from-request
             (node-create-relationship from)
             (json->string
              `((to . ,(node-self to))
                (type . ,type)
                (data . ,data)))
             read-json)))
       (apply make-relationship (alist-values relationship))))))

 ;; Relationship or relationship-self (i.e. relationship-id)? It may
 ;; be more common to pass around the ids; or, check this: we can
 ;; accept either, and extract in the case of objects!
 ;;
 ;; We're going to be dealing with a constant object/object-self pun,
 ;; though, which might become a pain-in-the-ass; hmm.
 (define (delete-relationship relationship)
   (with-input-from-request
    (make-request method: 'DELETE
                  uri: (uri-reference (relationship-self relationship)))
    #f
    values))

 ;; Should it be `relationship-properties-all'?
 (define (get-all-relationship-properties relationship)
   (with-input-from-request
    (relationship-properties relationship)
    #f
    values))

 (define (set-all-relationship-properties relationship properties)
   (with-input-from-request
    (make-request method: 'PUT
                  uri: (uri-reference (relationship-properties relationship)))
    (json->string properties)
    read-json))

 (define (replace template key value)
   (irregex-replace `(: "{" ,key "}") template value))

 (define (get-relationship-property relationship key)
   (with-input-from-request
    (replace (relationship-property relationship)
             "key"
             (symbol->string key))
    #f
    ;; Questionable? read-json doesn't seem to work.
    read))

 (define (set-relationship-property relationship key value)
   (with-input-from-request
    (make-request method: 'PUT
                  uri: (uri-reference
                        (replace (relationship-property relationship)
                                 "key"
                                 (symbol->string key))))
    (json->string value)
    values))

 (define (get-all-relationships node)
   (map (lambda (relationship)
          (apply make-relationship relationship))
        (with-input-from-request
         (node-all-relationships node)
         #f
         read-json)))

 (define (get-incoming-relationships node)
   (map (lambda (relationship)
          (apply make-relationship relationship))
        (with-input-from-request
         (node-incoming-relationships node)
         #f
         read-json)))

 (define (get-outgoing-relationships node)
   (map (lambda (relationship)
          (apply make-relationship relationship))
        (with-input-from-request
         (node-outgoing-relationships node)
         #f
         read-json)))

 (define (get-all-typed-relationships node types)
   (map (lambda (relationship)
          (apply make-relationship relationship))
        (with-input-from-request
         (replace
          (node-all-typed-relationships node)
          "-list|&|types"
          (string-join (map uri-encode-string types) "&"))
         #f
         read-json)))

 (define (get-incoming-typed-relationships node types)
   (map (lambda (relationship)
          (apply make-relationship relationship))
        (with-input-from-request
         (replace
          (node-incoming-typed-relationships node)
          "-list|&|types"
          (string-join (map uri-encode-string types) "&"))
         #f
         read-json)))

 (define (get-outgoing-typed-relationships node types)
   (map (lambda (relationship)
          (apply make-relationship relationship))
        (with-input-from-request
         (replace
          (node-outgoing-typed-relationships node)
          "-list|&|types"
          (string-join (map uri-encode-string types) "&"))
         #f
         read-json)))

 (define (get-relationship-types service-root)
   (with-input-from-request
    (service-root-relationship-types service-root)
    #f
    read-json))

 (define (get-node-property node key)
   (with-input-from-request
    (replace
     (node-property node)
     "key"
     key)
    #f
    read))

 (define (set-all-node-properties node properties)
   (with-input-from-request
    (make-request method: 'PUT
                  uri: (uri-reference (node-properties node)))
    properties
    read-json)
   (void))

 (define (get-all-node-properties node)
   (receive (properties uri response)
     (with-input-from-request
      (node-properties node)
      #f
      read-json)
     properties))

 (define (delete-all-node-properties node)
   (with-input-from-request
    (make-request method: 'DELETE
                  uri: (uri-reference (node-properties node)))
    #f
    read-json))

 (define (delete-node-property node)
   (with-input-from-request
    (make-request method: 'DELETE
                  uri: (uri-reference (node-property node)))
    #f
    read-json))

 (define (delete-relationship-property relationship key)
   (with-input-from-request
    (make-request method: 'DELETE
                  uri: (uri-reference
                        (replace
                         (relationship-property relationship)
                         "key"
                         (symbol->string key))))
    #f
    read-json)
   (void)))
