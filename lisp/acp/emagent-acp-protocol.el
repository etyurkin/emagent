;;; emagent-acp-protocol.el --- ACP protocol layer for emagent -*- lexical-binding: t; -*-

;; Author: Evgeniy Tyurkin <etyurkin@kwarks.org>
;; SPDX-License-Identifier: MIT
;; Version: 1.2.8
;; This file is part of emagent.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Self-contained implementation of the Agent Communication Protocol (ACP)
;; for emagent.  Symbols use the emagent-acp- prefix so this file can
;; coexist with xenodium/acp.el in the same Emacs session.
;;
;; Implementation is split across require-DAG leaves:
;;   `emagent-acp-protocol-log'      — defgroup and wire logging
;;   `emagent-acp-protocol-json'     — JSON helpers and error constructors
;;   `emagent-acp-protocol-client'   — client object and send APIs
;;   `emagent-acp-protocol-wire'     — request/response/notification routing
;;   `emagent-acp-protocol-requests' — request/response builders
;;
;; See https://agentclientprotocol.com for the ACP specification.

;;; Code:

(require 'emagent-acp-protocol-log)
(require 'emagent-acp-protocol-json)
(require 'emagent-acp-protocol-client)
(require 'emagent-acp-protocol-wire)
(require 'emagent-acp-protocol-requests)

(provide 'emagent-acp-protocol)
;;; emagent-acp-protocol.el ends here
