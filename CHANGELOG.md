# Revision history for gemini-textboard

## 0.2.0.1 -- 2021-03-19

* Update gemini-server dependency (TLS resumption fix)

## 0.2.0.0 -- 2021-02-22

* SSL/TLS
  - stunnel is no longer necessary
  - when present, the client certificate is used to derive a hash pseudonym
    (similar to tripcodes in classic boards)

## 0.1.0.0 -- 2020-07-24

* First version. Released on an unsuspecting world.
