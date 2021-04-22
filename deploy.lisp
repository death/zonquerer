(ql:quickload "deploy")

(deploy:define-resource-directory assets #p"assets/")

(asdf:make "zonquerer")
