(defsystem "mindmap"
  :depends-on (:postmodern
               :split-sequence
               :jonathan
               :com.inuoe.jzon
               :clack
               :woo
               :websocket-driver-server
               :local-time
               :frugal-uuid
               :babel
               :ironclad
               :cl-base64
               :cl-dotenv)
  :serial t
  :components (
                (:file "controllers/websocket-package")
                (:file "utils/utils")
                (:file "utils/verify")
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
                (:file "utils/server-utils")
                (:file "controllers/server")
                (:file "main"))) ;; ← エントリーポイント
