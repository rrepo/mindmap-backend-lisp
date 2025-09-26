(defsystem "mindmap"
  :depends-on (:postmodern
               :split-sequence
               :jonathan
               :clack
               :woo
               :websocket-driver-server
               :jsown
               :local-time
               :frugal-uuid
               :ironclad
               :cl-base64
               :cl-dotenv)
  :serial t
  :components (
                (:file "utils/utils")
                (:file "models/initsql")
                (:file "models/users")
                (:file "models/maps")
                (:file "models/nodes")
                (:file "models/map-members")
                (:file "models/map-invitations")
                (:file "services/mindmaps")
                (:file "controllers/users")
                (:file "controllers/maps")
                (:file "controllers/nodes")
                (:file "controllers/map-members")
                (:file "controllers/map-invitations")
                (:file "utils/env")
                (:file "controllers/server")
                (:file "main"))) ;; ← エントリーポイント
