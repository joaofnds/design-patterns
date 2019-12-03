;;;; What is the Facade Pattern?
;;;;
;;;; - When you create a simplified interface that performs many
;;;;   other actions behind the scenes
;;;;
;;;; Ex:
;;;;   - Can I withdrawal $50 from the bank?
;;;;   - Check if the checking account is valid
;;;;   - Chceck if security code is valid
;;;;   - Chceck if fund are available
;;;;   - Make changes accordingly
;;;;
;;;; @see http://www.newthinktank.com/2012/09/facade-design-pattern-tutorial/

(defclass welcome-bank () ())

(defclass account-number-check () ())
(defclass security-code-check () ())

(defgeneric account-active-p (validator account-number))
(defmethod account-active-p ((validator account-number-check) account-number)
  (= account-number 12345678))

(defgeneric correct-code-p (validator code))
(defmethod correct-code-p ((validator security-code-check) code)
  (= code 1234))

(defclass funds-check ()
  ((cash :initform 1000 :reader cash)))

(defun decrease-cash-in-account (funds-check cash-to-withdraw)
  (with-slots (cash) funds-check
    (decf cash cash-to-withdraw)))


(defun increase-cash-in-account (funds-check cash-to-withdraw)
  (with-slots (cash) funds-check
    (incf cash cash-to-withdraw)))

(defun enough-money-p (funds-check cash-to-withdraw)
  (with-slots (cash) funds-check
    (if (> cash-to-withdraw cash)
        (progn
          (format t "[error] you don't have enough money~%")
          (format t "[error] current balance: ~a~%" cash)
          nil)
        (progn
          (decrease-cash-in-account funds-check cash-to-withdraw)
          (format t "[withdraw complete] current balance is: ~a~%" cash)
          t))))

(defun make-deposit (funds-check cash-to-deposit)
  (increase-cash-in-account funds-check cash-to-deposit)
  (format t "[deposit complete] current balance is: ~a~%" (slot-value funds-check 'cash)))

(defclass bank-account-facade ()
  ((account-number  :initform (error "must have account-number") :initarg :account-number :reader account-number)
   (security-code   :initform (error "must have security-code")  :initarg :security-code  :reader security-code)
   (bank            :initform (make-instance 'welcome-bank))
   (account-checker :initform (make-instance 'account-number-check))
   (code-checker    :initform (make-instance 'security-code-check))
   (funds-checker   :initform (make-instance 'funds-check))))

(defun withdraw-cash (bank-account-facade cash)
  (with-slots (account-number
               security-code
               account-checker
               code-checker
               funds-checker) bank-account-facade
    (if (and
         (account-active-p account-checker account-number)
         (correct-code-p code-checker security-code)
         (enough-money-p funds-checker cash))
        (format t "transaction complete~%")
        (format t "trasaction failed~%"))))

(defun deposit-cash (bank-account-facade cash)
  (with-slots (account-number
               security-code
               account-checker
               code-checker
               funds-checker) bank-account-facade
    (if (and
         (account-active-p account-checker account-number)
         (correct-code-p code-checker security-code)
         (make-deposit funds-checker cash))
        (format t "transaction complete~%")
        (format t "trasaction failed~%"))))

;;;; ---------------------------------------------------------------------------

(defvar *bank* (make-instance 'bank-account-facade :account-number 12345678 :security-code 1234))
(withdraw-cash *bank* 500)
(withdraw-cash *bank* 9000)
(deposit-cash *bank* 2000)
(withdraw-cash *bank* 500)
