#lang at-exp racket/base

;; Generate a form email to let someone know their SPF records are misconfigured for their current email provider.
;;
;; Run (fill-report "domain.com" "1.2.3.4") where the 2nd arg is the sending email server's IP address.
;; It will copy the completed report to the clipboard for you.

;; Only works on Windows for now.

(require "options.rkt"
         net/dns
         racket/class
         racket/format
         racket/gui/base
         racket/list
         racket/match
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

(provide (all-defined-out))

(define-logger canned)
(define (info v) (log-canned-info (format "→ ~a" v)) v)

;;
;; Options
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-runtime-path options.ini "options.ini")
(define opts (load-options options.ini))

(define my-company-name (hash-ref opts 'company-name))
(define my-direct-phone (hash-ref opts 'direct-phone))

;;
;; Email provider definitions
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(struct provider (name spf doc-link)
  #:methods gen:custom-write
  [(define (write-proc p port mode)
     (write-string (format "<provider: ~a>" (provider-name p)) port))])

;; Common email providers and their required SPF directives. Will build this up over time.
(define server-providers
  (hash
   "google\\.com"   (provider "Google Domains"
                              "include:_spf.google.com"
                              "https://support.google.com/a/answer/10685031")
   "pp(?:e\\-)hosted\\.com" (provider "Proofpoint"
                                      "a:dispatch-us.ppe-hosted.com"
                                      "https://help.proofpoint.com/Proofpoint_Essentials/Email_Security/Administrator_Topics/000_gettingstarted/020_connectiondetails")
   "pobox\\.com"    (provider "Pobox"
                              "include:pobox.com"
                              "https://www.pobox.help/hc/en-us/articles/360060504613-Setting-up-SPF-for-your-domain")
   "livemail\\.co\\.uk" (provider "Fasthosts Livemail"
                                  "a ip4:213.171.216.0/24 ip4:77.68.64.0/27 mx"
                                  "https://help.fasthosts.co.uk/app/answers/detail/a_id/519/~/adding-a-spf-record-to-your-domains-dns")
   "yandex\\.net"   (provider "Yandex"
                              "include:_spf.yandex.net"
                              "https://yandex.com/support/connect/dns/spf.html")
   "outlook\\.com"  (provider "Microsoft 365"
                              "include:spf.protection.outlook.com"
                              "https://docs.microsoft.com/en-us/microsoft-365/security/office-365-security/set-up-spf-in-office-365-to-help-prevent-spoofing?view=o365-worldwide")
   "protection\\.office365\\.us" (provider "Microsoft 365 GCC High"
                                           "include:spf.protection.office365.us"
                                           "https://docs.microsoft.com/en-us/microsoft-365/enterprise/dns-records-for-office-365-gcc-high")
   "securence\\.com" (provider "Securence"
                               "include:spf.securence.com"
                               "This is based on recent SPF records from other companies that use Securence; you should confirm this with Securence support.")
   "cloudfilter\\.net" (provider "BlueHost"
                                 "include:bluehost.com"
                                 "https://www.bluehost.com/help/article/dns-spf")
   "\\.qq\\.com" (provider "QQ.com"
                           "include:spf.mail.qq.com"
                           "https://service.mail.qq.com/cgi-bin/help?subtype=1&no=1001505&id=16"))
  )

(define (server->provider domain)
  (for/or ([pat (in-hash-keys server-providers)])
    (and (regexp-match? (regexp pat) domain) (hash-ref server-providers pat))))

;;
;; Lookups
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (run+parse executable args pat [selector-proc car])
  (define exe-path
    (match (find-executable-path executable #f)
      [(? path? p) p]
      [_ (raise-user-error 'run+parse "could not find path for executable ~a" executable)]))
  (define cmd (format "~a ~a" exe-path args))
  (define result
    (parameterize ([current-error-port (open-output-string)]) ; throw away any error output
      (with-output-to-string (lambda () (system cmd)))))
  (regexp-match* pat result #:match-select selector-proc))

; This only works on Windows for now
(define (get-spf domain)
  (log-canned-info "Getting current SPF record for ~a…" domain)
  (info (match (run+parse "nslookup.exe" (format "-type=txt ~a" domain) #rx"(v=spf1[^\"]+)")
          [(list* (? string? spf) _) spf]
          [_ "SPF NOT FOUND"])))

(define (get-mx-service domain)
  (server->provider (string-downcase (dns-get-mail-exchanger (dns-find-nameserver) domain))))

(define (ipv4->provider ip-str)
  (define server (reverse-lookup ip-str))
  (info (or (server->provider server)
            (provider (format "Unknown: ~a" server) "[unknown]" "[unknown]"))))

(define (get-registrar domain)
  (log-canned-info "WHOIS lookup to find registrar for ~a…" domain)
  (info (match (run+parse "whois.exe" (format "-v ~a" domain) #rx"Registrar: (?m:(.+)$)" last)
          [(list (? string? results) ...) #:when (> (length results) 0) (string-trim (car results))]
          [_ "NOT FOUND"])))

(define (reverse-lookup ip-addr)
  (log-canned-info "Doing reverse lookup on ~a…" ip-addr)
  (info (dns-get-name (dns-find-nameserver) ip-addr)))

;;
;; Report
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (replace-report-fields report fields)
  (regexp-replace* (regexp (string-join (hash-keys fields) "|"))
                   report
                   (lambda (match-str) (hash-ref fields match-str match-str))))

(define (fill-report domain sender-ip)
  (define spf (get-spf domain))
  (match-define (provider mail-provider-name provider-spf provider-docs) (ipv4->provider sender-ip))
  (define registrar (get-registrar domain))

  (define next-steps
    (if (regexp-match? provider-spf "unknown")
        next-steps-unknown
        (replace-report-fields next-steps-known
                               (hash "{DIRECTIVE}" provider-spf "{DOCS}" provider-docs "{SERVICE}" mail-provider-name))))
  
  (define report-fields
    (hash "{COMPANY}" my-company-name
          "{MY-PHONE}" my-direct-phone
          "{DOMAIN}" domain
          "{SENDER-IP}" sender-ip
          "{SPFRECORD}" spf
          "{SERVICE}" mail-provider-name
          "{DIRECTIVE}" provider-spf
          "{DOCS}" provider-docs
          "{NEXTSTEPS}" next-steps
          "{REGISTRAR}" registrar))

  (define filled-report (replace-report-fields report report-fields))
  (info (format "\n~a" filled-report))
  (send the-clipboard set-clipboard-string filled-report 0))

(define next-steps-known @~a{
Since your organization sends email via {SERVICE}, you MUST include the directive {DIRECTIVE} in your SPF record. (See their documentation at {DOCS})
})

(define next-steps-unknown @~a{
You need to amend your SPF record to cover all the IP addresses used by your email provider. Most email services have public documentation specifying what directives their clients should include in their SPF records. I searched, but I was unable to find any such documentation from your email provider. You should contact their tech support and ask them what SPF directives you should add that would include all their outgoing email servers.
})
  
(define report @~a{
Hello,

I’m the IT admin at {COMPANY}. We’re having trouble receiving emails from your organization; they are being sent from a server that {DOMAIN} does not list as one of its legitimate servers. When this happens, the emails are marked as Fraud and cannot be released for delivery by anyone except a system administrator.

I’ve included some details about the problem and how to fix it below. If you do not have access to your company’s DNS records, please forward this email to someone in an IT role. If anyone would like to contact me directly, I would be happy to help: my direct number is {MY-PHONE}. (A phone call might be best in this case — because of this email issue.)

This problem is likely hurting the deliverability of your emails to other companies as well. SPF checks are one of the basic ways that companies are increasingly using to combat spam and phishing threats.

More details:
Some or all of the emails being sent from {DOMAIN} to {COMPANY} are being delivered by a server with the IP address {SENDER-IP}, but this address is not listed as a legitimate server in the SPF records for {DOMAIN}.

{DOMAIN}'s current SPF record is: {SPFRECORD}
Your email headers indicate that your organization is using {SERVICE} to send your emails, and indeed, your SPF record shown above is missing some or all of the servers used by {SERVICE}.

Suggested actions:
{NEXTSTEPS}

The {DOMAIN} domain appears to be registered through “{REGISTRAR}”. The person at your organization with access to the {REGISTRAR} account for the {DOMAIN} domain should log in and update the above SPF record.
})