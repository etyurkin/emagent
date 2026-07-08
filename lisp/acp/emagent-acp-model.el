;;; emagent-acp-model.el --- ACP model configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Evgeniy Tyurkin

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Model entry parsing, config-option management, model resolution, and
;; the configure-model flow for the ACP layer.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'emagent-log)
(require 'emagent-acp-custom)

(defvar emagent-model-history nil
  "Minibuffer history for agent/model choices in
`emagent-acp--read-labeled-choice'.")

(declare-function emagent-acp--send-request "emagent-acp")
(require 'emagent-model)
(declare-function emagent-acp--saved-model-id "emagent-acp-usage")

(defun emagent-acp--auto-model-candidate (state models)
  "Return the agent's auto/default model id when advertised."
  (or (and (emagent-acp--model-available-p "default[]" state models) "default[]")
      (and (emagent-acp--model-available-p emagent-acp-auto-model-id state models)
           emagent-acp-auto-model-id)))
(declare-function emagent-acp--session-ready "emagent-acp")
(declare-function emagent-acp--progress "emagent-acp")
(declare-function emagent-acp-make-session-set-config-option-request "emagent-acp-protocol" t)

(defun emagent-acp--normalize-config-option (emagent-acp-option)
  `((:id . ,(map-elt emagent-acp-option 'id))
    (:name . ,(map-elt emagent-acp-option 'name))
    (:description . ,(map-elt emagent-acp-option 'description))
    (:category . ,(map-elt emagent-acp-option 'category))
    (:type . ,(map-elt emagent-acp-option 'type))
    (:current-value . ,(map-elt emagent-acp-option 'currentValue))
    (:options . ,(mapcar (lambda (emagent-acp-value)
                           `((:value . ,(map-elt emagent-acp-value 'value))
                             (:name . ,(map-elt emagent-acp-value 'name))
                             (:description . ,(map-elt emagent-acp-value 'description))))
                         (append (map-elt emagent-acp-option 'options) nil)))))

(defun emagent-acp--normalize-config-options (emagent-acp-config-options)
  (mapcar #'emagent-acp--normalize-config-option
          (append emagent-acp-config-options nil)))

(defun emagent-acp--save-config-options (state emagent-acp-config-options)
  (when emagent-acp-config-options
    (setf (emagent-acp-state-config-options state)
              (emagent-acp--normalize-config-options emagent-acp-config-options))))

(defun emagent-acp--config-options (state)
  (emagent-acp-state-config-options state))

(defun emagent-acp--config-option-by-category (state category)
  (seq-find (lambda (option)
              (equal category (map-elt option :category)))
            (emagent-acp--config-options state)))

(defun emagent-acp--model-config-option (state)
  (or (emagent-acp--config-option-by-category state "model")
      (seq-find (lambda (option)
                  (string= (map-elt option :id) "model"))
                (emagent-acp--config-options state))))

(defun emagent-acp--config-option-value-name (option value)
  (or (map-elt (seq-find (lambda (candidate)
                           (equal value (map-elt candidate :value)))
                         (map-elt option :options))
               :name)
      value))

(defun emagent-acp--config-option-set-value (state config-id value)
  (dolist (option (emagent-acp--config-options state))
    (when (equal config-id (map-elt option :id))
      (setf (map-elt option :current-value) value))))

(defun emagent-acp--read-labeled-choice (prompt labels &optional _default-label)
  "Read one of LABELS with `completing-read'.

Uses a completion table so `value' accepts only an exact label — the
highlighted Vertico candidate is returned, not a partial filter string.
LABELS may include text properties (e.g. faces) for display."
  (let* ((labels (copy-sequence labels))
         (plain-labels (mapcar #'substring-no-properties labels))
         (collection (lambda (input _predicate action)
                       (pcase action
                         ('value
                          (let ((input (substring-no-properties input)))
                            (when (member input plain-labels) (list input))))
                         (_ (all-completions input labels)))))
         (selection (substring-no-properties
                     (completing-read prompt collection nil t nil
                                      'emagent-model-history))))
    (unless (member selection plain-labels)
      (user-error "Invalid choice: %s" selection))
    selection))

(defun emagent-acp--model-entry-id (entry)
  (or (map-elt entry 'modelId) (map-elt entry 'model-id) (map-elt entry 'value)))

(defun emagent-acp--model-entry-name (entry)
  (or (map-elt entry 'name) (emagent-acp--model-entry-id entry)))

(defun emagent-acp--model-entries-from-response (response)
  "Return a list of ((:model-id . ID) (:name . NAME)) from a session/new RESPONSE."
  (when response
    (let* ((models (emagent-acp--models-from-response response))
           (entries (append (emagent-acp--available-model-entries models) nil)))
      (delq nil
            (mapcar (lambda (entry)
                      (let ((id (or (map-elt entry :model-id)
                                    (map-elt entry :value)
                                    (emagent-acp--model-entry-id entry)))
                            (name (or (map-elt entry :name)
                                      (emagent-acp--model-entry-name entry))))
                        (when id
                          `((:model-id . ,id) (:name . ,name)))))
                    entries)))))

(defun emagent-acp--models-from-response (response)
  "Extract model list from session/new RESPONSE.
Prefer `configOptions' (canonical ids for set-config-option)
over legacy `models'."
  (or (when-let* ((options (map-elt response 'configOptions))
                  (model-opt
                   (seq-find (lambda (option)
                               (or (string= (map-elt option 'category) "model")
                                   (string= (map-elt option 'id) "model")))
                             options)))
        (list (cons 'availableModels (map-elt model-opt 'options))
              (cons 'currentModelId (map-elt model-opt 'currentValue))))
      (map-elt response 'models)))

(defun emagent-acp--available-model-entries (models)
  (or (map-elt models 'availableModels) (map-elt models 'options) nil))

(defun emagent-acp--get-available-models (state models)
  (if-let ((model-option (emagent-acp--model-config-option state)))
      (mapcar (lambda (value)
                `((:model-id . ,(map-elt value :value))
                  (:name . ,(map-elt value :name))
                  (:description . ,(map-elt value :description))))
              (map-elt model-option :options))
    (emagent-acp--available-model-entries models)))

(defun emagent-acp--current-model-id (state models)
  (or (map-elt (emagent-acp--model-config-option state) :current-value)
      (and models (map-elt models 'currentModelId))
      (emagent-acp--saved-model-id state)))

(defun emagent-acp--model-display-name (state models model-id)
  (or (map-elt (seq-find (lambda (model)
                           (string= (or (map-elt model :model-id)
                                        (emagent-acp--model-entry-id model))
                                    model-id))
                         (emagent-acp--get-available-models state models))
               :name)
      (emagent-acp--model-entry-name
       (seq-find (lambda (model)
                   (string= (emagent-acp--model-entry-id model) model-id))
                 (emagent-acp--get-available-models state models)))
      model-id))

(defun emagent-acp--model-choices (state models)
  (mapcar (lambda (entry)
            (let ((id (or (map-elt entry :model-id)
                          (emagent-acp--model-entry-id entry)))
                  (name (or (map-elt entry :name)
                            (emagent-acp--model-entry-name entry))))
              (cons (emagent-model-choice-label-display id name) id)))
          (emagent-acp--get-available-models state models)))

(defun emagent-acp--model-available-p (model-id state models)
  (and model-id (not (string-empty-p model-id))
       (seq-find (lambda (entry)
                   (string= model-id
                            (or (map-elt entry :model-id)
                                (emagent-acp--model-entry-id entry))))
                 (emagent-acp--get-available-models state models))))

(defun emagent-acp--match-model-id (model-id state models)
  "Return canonical MODEL-ID for set-config-option, matching by id or name."
  (when (and model-id (not (string-empty-p model-id)))
    (let ((model-id (emagent-model-canonical-id model-id)))
      (or (and (emagent-acp--model-available-p model-id state models) model-id)
          (cl-loop for entry across (vconcat (emagent-acp--get-available-models state models))
                   for id = (or (map-elt entry :model-id)
                                (emagent-acp--model-entry-id entry))
                   for name = (or (map-elt entry :name)
                                  (emagent-acp--model-entry-name entry))
                   when (or (string= id model-id)
                            (string= name model-id)
                            (string= (downcase name) (downcase model-id))
                            (string= (emagent-model-normalize-id id)
                                     (emagent-model-normalize-id model-id)))
                   return id)
          model-id))))

(defun emagent-acp--resolve-model-id (state models saved-model-id)
  "Return a model id for session connect without prompting.

Prefers the buffer's saved model (from startup selection), then \"auto\"
when advertised, then the agent's current model."
  (let* ((available (emagent-acp--get-available-models state models))
         (current (and models (map-elt models 'currentModelId))))
    (cond
     ((and saved-model-id (not (string-empty-p saved-model-id)))
      (emagent-acp--match-model-id saved-model-id state models))
     ((emagent-acp--auto-model-candidate state models))
     ((and current (not (string-empty-p current))) current)
     ((= (length available) 1)
      (or (map-elt (car available) :model-id)
          (emagent-acp--model-entry-id (car available))))
     (t nil))))

(cl-defun emagent-acp--config-option-set-model-id (&key state session-id model-id
                                                        on-success on-failure)
  (if-let ((model-option (emagent-acp--model-config-option state)))
      (emagent-acp--send-request
       :state state
       :request (emagent-acp-make-session-set-config-option-request
                 :session-id session-id
                 :config-id (map-elt model-option :id)
                 :value model-id)
       :on-success (lambda (response)
                     (if (map-elt response 'configOptions)
                         (emagent-acp--save-config-options state
                                                           (map-elt response 'configOptions))
                       (emagent-acp--config-option-set-value state
                                                             (map-elt model-option :id)
                                                             model-id))
                     (emagent-acp--persist-model-id state model-id)
                     (emagent-acp--progress
                      state
                      (format "model %s"
                              (emagent-acp--config-option-value-name model-option model-id)))
                     (when on-success (funcall on-success)))
       :on-failure (lambda (error _raw)
                     (emagent-acp--notify-user
                      state
                      (format "emagent: model %s not applied: %s"
                              model-id
                              (or (map-elt error 'message) (format "%s" error))))
                     (when on-failure (funcall on-failure))))
    (emagent-acp--send-request
     :state state
     :request (emagent-acp-make-session-set-model-request
               :session-id session-id
               :model-id model-id)
     :on-success (lambda (_response)
                   (emagent-acp--persist-model-id state model-id)
                   (emagent-acp--notify-user
                    state
                    (format "emagent: model %s" model-id))
                   (when on-success (funcall on-success)))
     :on-failure (lambda (error _raw)
                   (emagent-acp--notify-user
                    state
                    (format "emagent: model %s not applied: %s"
                            model-id
                            (or (map-elt error 'message) (format "%s" error))))
                   (when on-failure (funcall on-failure))))))

(defun emagent-acp--finish-configure-model (state session-id on-ready resumed)
  (emagent-acp--session-ready
   :state state
   :session-id session-id
   :on-ready on-ready
   :resumed resumed))

(cl-defun emagent-acp--configure-model (&key state session-id response on-ready resumed)
  (emagent-acp--progress state "selecting model…")
  (emagent-acp--save-config-options state (map-elt response 'configOptions))
  (let* ((models (emagent-acp--models-from-response response))
         (current (emagent-acp--current-model-id state models))
         (choice (emagent-acp--resolve-model-id state models
                                                (emagent-acp--saved-model-id state))))
    (cond
     ((and choice session-id (not (string-empty-p choice))
           current (string= choice current))
      (emagent-acp--progress
       state
       (format "model %s"
               (emagent-acp--model-display-name state models choice)))
      (emagent-acp--persist-model-id state choice)
      (emagent-acp--finish-configure-model state session-id on-ready resumed))
     ((and choice session-id (not (string-empty-p choice)))
      (emagent-acp--progress
       state
       (format "setting model to %s…"
               (emagent-acp--model-display-name state models choice)))
      (emagent-acp--config-option-set-model-id
       :state state
       :session-id session-id
       :model-id choice
       :on-success (lambda ()
                     (emagent-acp--finish-configure-model state session-id on-ready resumed))
       :on-failure (lambda ()
                     (emagent-acp--finish-configure-model state session-id on-ready resumed))))
     (t
      (when current
        (emagent-acp--progress
         state
         (format "model %s"
                 (emagent-acp--model-display-name state models current)))
        (emagent-acp--persist-model-id state current))
      (emagent-acp--finish-configure-model state session-id on-ready resumed)))))

(provide 'emagent-acp-model)
;;; emagent-acp-model.el ends here
