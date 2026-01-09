<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# yandex-metrika - A client
library for metrika.yandex.ru

<a id="yandex-metrika-asdf-system-details"></a>

## YANDEX-METRIKA ASDF System Details

* Description: A client
library for metrika.yandex.ru
* Licence: Unlicense
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants.com/yandex-metrika][a6d0]
* Bug tracker: [https://github.com/40ants/yandex-metrika/issues][5397]
* Source control: [GIT][957b]
* Depends on: [alexandria][8236], [cl-interpol][f8a9], [dexador][8347], [jonathan][6dd8], [lisp-stat][2105], [local-time][46a1], [log4cl-extras][691c], [secret-values][cd18], [serapeum][c41d], [str][ef7f], [yason][aba2]

[![](https://github-actions.40ants.com/40ants/yandex-metrika/matrix.svg?only=ci.run-tests)][edce]

![](http://quickdocs.org/badge/yandex-metrika.svg)

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :yandex-metrika)
```
<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

`TODO`: Write a library description. Put some examples here.

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40YANDEX-METRIKA-2FCLIENT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### YANDEX-METRIKA/CLIENT

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-22YANDEX-METRIKA-2FCLIENT-22-29-20PACKAGE-29"></a>

#### [package](d094) `yandex-metrika/client`

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FCLIENT-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28YANDEX-METRIKA-2FCLIENT-3AHIT-20FUNCTION-29"></a>

##### [function](d6a4) `yandex-metrika/client:hit` url &key params user-id (timeout 1)

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FCLIENT-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-28YANDEX-METRIKA-2FVARS-3A-2ACOUNTER-2A-20-28VARIABLE-29-29"></a>

##### [variable](25e9) `yandex-metrika/vars:*counter*` nil

Yandex Metrika's counter id.

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40YANDEX-METRIKA-2FLOGS-2FAPI-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### YANDEX-METRIKA/LOGS/API

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22YANDEX-METRIKA-2FLOGS-2FAPI-22-29-20PACKAGE-29"></a>

#### [package](90ae) `yandex-metrika/logs/api`

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FLOGS-2FAPI-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40YANDEX-METRIKA-2FLOGS-2FAPI-24LOGS-API-ERROR-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### LOGS-API-ERROR

<a id="x-28YANDEX-METRIKA-2FLOGS-2FAPI-3ALOGS-API-ERROR-20CONDITION-29"></a>

###### [condition](00b5) `yandex-metrika/logs/api:logs-api-error` (error)

**Readers**

<a id="x-28YANDEX-METRIKA-2FLOGS-2FAPI-3ALOGS-API-ERROR-CODE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FAPI-3ALOGS-API-ERROR-29-29"></a>

###### [reader](00b5) `yandex-metrika/logs/api:logs-api-error-code` (logs-api-error) (:code)

<a id="x-28YANDEX-METRIKA-2FLOGS-2FAPI-3ALOGS-API-ERROR-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FAPI-3ALOGS-API-ERROR-29-29"></a>

###### [reader](00b5) `yandex-metrika/logs/api:logs-api-error-message` (logs-api-error) (:message)

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FLOGS-2FAPI-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28YANDEX-METRIKA-2FLOGS-2FAPI-3AAPI-GET-20FUNCTION-29"></a>

##### [function](0d05) `yandex-metrika/logs/api:api-get` path &key params

Make `GET` request to Logs `API`.
`PATH` is the `API` endpoint path (e.g., "/logrequests").
`PARAMS` is an alist of query parameters.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FAPI-3AAPI-POST-20FUNCTION-29"></a>

##### [function](7cbf) `yandex-metrika/logs/api:api-post` path &key params content

Make `POST` request to Logs `API`.
`PATH` is the `API` endpoint path.
`PARAMS` is an alist of query parameters.
`CONTENT` is the request body (will be `JSON`-encoded if provided as hash-table).

<a id="x-28YANDEX-METRIKA-2FLOGS-2FAPI-3AFORMAT-DATE-20FUNCTION-29"></a>

##### [function](64c2) `yandex-metrika/logs/api:format-date` date

Format a date for the `API` (`YYYY-MM-DD` format).
`DATE` can be a local-time:timestamp or a string.

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FLOGS-2FAPI-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-28YANDEX-METRIKA-2FLOGS-2FAPI-3A-2AAPI-BASE-URL-2A-20-28VARIABLE-29-29"></a>

##### [variable](b65c) `yandex-metrika/logs/api:*api-base-url*` "https://api-metrica.yandex.net/management/v1/counter"

Base `URL` for Yandex Metrika Management `API`.

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40YANDEX-METRIKA-2FLOGS-2FDOWNLOAD-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### YANDEX-METRIKA/LOGS/DOWNLOAD

<a id="x-28-23A-28-2828-29-20BASE-CHAR-20-2E-20-22YANDEX-METRIKA-2FLOGS-2FDOWNLOAD-22-29-20PACKAGE-29"></a>

#### [package](ee94) `yandex-metrika/logs/download`

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FLOGS-2FDOWNLOAD-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28YANDEX-METRIKA-2FLOGS-2FDOWNLOAD-3ADOWNLOAD-ALL-PARTS-20FUNCTION-29"></a>

##### [function](f414) `yandex-metrika/logs/download:download-all-parts` request

Download all parts of a processed log request.
`REQUEST` is a [`log-request`][63c9] object.
Returns a list of `TSV` data strings, one per part.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FDOWNLOAD-3ADOWNLOAD-PART-20FUNCTION-29"></a>

##### [function](9596) `yandex-metrika/logs/download:download-part` request part-number

Download a specific part of a log request.
`REQUEST` is a [`log-request`][63c9] object.
`PART-NUMBER` is the part number to download.
Returns the raw `TSV` data as a string.

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40YANDEX-METRIKA-2FLOGS-2FFIELDS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### YANDEX-METRIKA/LOGS/FIELDS

<a id="x-28-23A-28-2826-29-20BASE-CHAR-20-2E-20-22YANDEX-METRIKA-2FLOGS-2FFIELDS-22-29-20PACKAGE-29"></a>

#### [package](01f9) `yandex-metrika/logs/fields`

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FLOGS-2FFIELDS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3AMAKE-FIELDS-STRING-20FUNCTION-29"></a>

##### [function](9d96) `yandex-metrika/logs/fields:make-fields-string` fields

Convert a list of field names to a comma-separated string.
`FIELDS` can be a list of strings or a single string.
Returns the fields as a comma-separated string suitable for `API` calls.

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FLOGS-2FFIELDS-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3A-2BHITS-BASIC-FIELDS-2B-20-28VARIABLE-29-29"></a>

##### [variable](a894) `yandex-metrika/logs/fields:+hits-basic-fields+` ("ym:pv:counterID" "ym:pv:dateTime" "ym:pv:date" "ym:pv:watchID" "ym:pv:clientID" "ym:pv:counterUserIDHash")

Basic hits fields for event identification.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3A-2BHITS-DEVICE-FIELDS-2B-20-28VARIABLE-29-29"></a>

##### [variable](d8e1) `yandex-metrika/logs/fields:+hits-device-fields+` ("ym:pv:deviceCategory" "ym:pv:operatingSystem" "ym:pv:operatingSystemRoot" "ym:pv:browser" "ym:pv:browserMajorVersion" "ym:pv:browserMinorVersion" "ym:pv:screenWidth" "ym:pv:screenHeight")

Device and browser related hits fields.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3A-2BHITS-GOAL-FIELDS-2B-20-28VARIABLE-29-29"></a>

##### [variable](37c0) `yandex-metrika/logs/fields:+hits-goal-fields+` ("ym:pv:goalsID")

Goal/conversion related hits fields.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3A-2BHITS-PAGE-FIELDS-2B-20-28VARIABLE-29-29"></a>

##### [variable](98aa) `yandex-metrika/logs/fields:+hits-page-fields+` ("ym:pv:URL" "ym:pv:title" "ym:pv:referer")

Page and `URL` related hits fields.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3A-2BHITS-UTM-FIELDS-2B-20-28VARIABLE-29-29"></a>

##### [variable](579d) `yandex-metrika/logs/fields:+hits-utm-fields+` ("ym:pv:UTMCampaign" "ym:pv:UTMContent" "ym:pv:UTMMedium" "ym:pv:UTMSource" "ym:pv:UTMTerm")

`UTM` related fields.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3A-2BVISITS-BASIC-FIELDS-2B-20-28VARIABLE-29-29"></a>

##### [variable](42b3) `yandex-metrika/logs/fields:+visits-basic-fields+` ("ym:s:counterID" "ym:s:dateTime" "ym:s:date" "ym:s:visitID" "ym:s:clientID" "ym:s:watchIDs" "ym:s:isNewUser" "ym:s:startURL" "ym:s:pageViews" "ym:s:visitDuration")

Basic visits fields for session identification and metrics.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3A-2BVISITS-DEVICE-FIELDS-2B-20-28VARIABLE-29-29"></a>

##### [variable](f6b2) `yandex-metrika/logs/fields:+visits-device-fields+` ("ym:s:deviceCategory" "ym:s:mobilePhone" "ym:s:mobilePhoneModel" "ym:s:operatingSystem" "ym:s:operatingSystemRoot" "ym:s:browser" "ym:s:browserLanguage" "ym:s:screenWidth" "ym:s:screenHeight" "ym:s:screenColors")

Device and browser related visits fields.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3A-2BVISITS-GEO-FIELDS-2B-20-28VARIABLE-29-29"></a>

##### [variable](e15b) `yandex-metrika/logs/fields:+visits-geo-fields+` ("ym:s:regionCountry" "ym:s:regionCity" "ym:s:regionCountryID" "ym:s:regionCityID" "ym:s:ipAddress")

Geographic related visits fields.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3A-2BVISITS-GOAL-FIELDS-2B-20-28VARIABLE-29-29"></a>

##### [variable](e9e4) `yandex-metrika/logs/fields:+visits-goal-fields+` ("ym:s:goalsID" "ym:s:goalsSerialNumber" "ym:s:goalsDateTime" "ym:s:goalsPrice" "ym:s:goalsOrder")

Goal/conversion related visits fields.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FFIELDS-3A-2BVISITS-TRAFFIC-FIELDS-2B-20-28VARIABLE-29-29"></a>

##### [variable](7344) `yandex-metrika/logs/fields:+visits-traffic-fields+` ("ym:s:trafficSource" "ym:s:lastTrafficSource" "ym:s:referer" "ym:s:searchEngineRoot" "ym:s:searchEngine" "ym:s:searchPhrase" "ym:s:UTMCampaign" "ym:s:UTMContent" "ym:s:UTMMedium" "ym:s:UTMSource" "ym:s:UTMTerm")

Traffic source related visits fields.

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40YANDEX-METRIKA-2FLOGS-2FREQUESTS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### YANDEX-METRIKA/LOGS/REQUESTS

<a id="x-28-23A-28-2828-29-20BASE-CHAR-20-2E-20-22YANDEX-METRIKA-2FLOGS-2FREQUESTS-22-29-20PACKAGE-29"></a>

#### [package](a3ff) `yandex-metrika/logs/requests`

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FLOGS-2FREQUESTS-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40YANDEX-METRIKA-2FLOGS-2FREQUESTS-24LOG-REQUEST-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### LOG-REQUEST

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-20CLASS-29"></a>

###### [class](375a) `yandex-metrika/logs/requests:log-request` ()

Represents a Yandex Metrika Logs `API` request.

**Readers**

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-ATTRIBUTION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-29-29"></a>

###### [reader](6aa4) `yandex-metrika/logs/requests:log-request-attribution` (log-request) (:attribution)

Attribution model used.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-COUNTER-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-29-29"></a>

###### [reader](b87e) `yandex-metrika/logs/requests:log-request-counter-id` (log-request) (:counter-id)

Counter `ID` this request belongs to.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-FIELDS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-29-29"></a>

###### [reader](11c6) `yandex-metrika/logs/requests:log-request-fields` (log-request) (:fields)

List of requested fields.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-FROM-DATE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-29-29"></a>

###### [reader](59c1) `yandex-metrika/logs/requests:log-request-from-date` (log-request) (:from-date)

Start date of the request period.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-ID-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-29-29"></a>

###### [reader](3fb2) `yandex-metrika/logs/requests:log-request-id` (log-request) (:request-id)

Unique identifier of the log request.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-PARTS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-29-29"></a>

###### [reader](23b5) `yandex-metrika/logs/requests:log-request-parts` (log-request) (:parts = nil)

List of available parts for download.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-SIZE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-29-29"></a>

###### [reader](206d) `yandex-metrika/logs/requests:log-request-size` (log-request) (:size)

Size of the prepared data in bytes.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-SOURCE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-29-29"></a>

###### [reader](1614) `yandex-metrika/logs/requests:log-request-source` (log-request) (:source)

Data source: :visits or :hits.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-STATUS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-29-29"></a>

###### [reader](cbcc) `yandex-metrika/logs/requests:log-request-status` (log-request) (:status)

Request status: :created, :processed, :canceled, etc.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-TO-DATE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-29-29"></a>

###### [reader](9bb9) `yandex-metrika/logs/requests:log-request-to-date` (log-request) (:to-date)

End date of the request period.

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FLOGS-2FREQUESTS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ACLEAN-REQUEST-20FUNCTION-29"></a>

##### [function](1613) `yandex-metrika/logs/requests:clean-request` request

Clean a log request to free up quota.
`REQUEST` is a [`log-request`][63c9] object.
Returns a new [`log-request`][63c9] object with updated status.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ACREATE-REQUEST-20FUNCTION-29"></a>

##### [function](8ee7) `yandex-metrika/logs/requests:create-request` source from-date to-date fields &key attribution

Create a new log request.
`SOURCE` is either :visits or :hits.
`FROM-DATE` and `TO-DATE` define the date range (can be timestamps or strings).
`FIELDS` is a list of field names to retrieve.
`ATTRIBUTION` is the attribution model (optional), one of:
  :first :last :lastsign :last-yandex-direct-click
  :cross-device-last-significant :cross-device-first
  :cross-device-last-yandex-direct-click :cross-device-last :automatic
Returns a [`log-request`][63c9] object.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3AEVALUATE-REQUEST-20FUNCTION-29"></a>

##### [function](82d2) `yandex-metrika/logs/requests:evaluate-request` source from-date to-date fields

Evaluate possibility of creating a log request.
`SOURCE` is either :visits or :hits.
`FROM-DATE` and `TO-DATE` define the date range (can be timestamps or strings).
`FIELDS` is a list of field names to retrieve.
Returns (`VALUES` possible
                max-possible-day-quantity
                expected-size
                log-request-sum-max-size
                log-request-sum-size).

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3AGET-REQUEST-20FUNCTION-29"></a>

##### [function](f919) `yandex-metrika/logs/requests:get-request` request

Get information about a specific log request.
`REQUEST` is either the `ID` of the log request (integer) or a [`log-request`][63c9] object.
Returns a [`log-request`][63c9] object with refreshed data from the `API`.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALIST-REQUESTS-20FUNCTION-29"></a>

##### [function](2fbf) `yandex-metrika/logs/requests:list-requests`

List all log requests for the current counter.
Returns a list of [`log-request`][63c9] objects.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3AN-DAYS-AGO-20FUNCTION-29"></a>

##### [function](af11) `yandex-metrika/logs/requests:n-days-ago` n

Return a timestamp for N days ago from today.
N must be a non-negative integer.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3AWAIT-FOR-REQUEST-20FUNCTION-29"></a>

##### [function](09df) `yandex-metrika/logs/requests:wait-for-request` request &key (interval 10) (timeout 3600)

Wait for a log request to be processed.
`REQUEST` is a [`log-request`][63c9] object.
`INTERVAL` is the polling interval in seconds (default 10).
`TIMEOUT` is the maximum wait time in seconds (default 3600 = 1 hour).
Returns T if the request is processed, `NIL` if timeout or failed.

<a id="x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3AYESTERDAY-20FUNCTION-29"></a>

##### [function](b999) `yandex-metrika/logs/requests:yesterday`

Return a timestamp for yesterday.
Useful as `TO-DATE` since today's data is incomplete.

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-40YANDEX-METRIKA-2FVARS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### YANDEX-METRIKA/VARS

<a id="x-28-23A-28-2819-29-20BASE-CHAR-20-2E-20-22YANDEX-METRIKA-2FVARS-22-29-20PACKAGE-29"></a>

#### [package](af24) `yandex-metrika/vars`

<a id="x-28YANDEX-METRIKA-DOCS-2FINDEX-3A-3A-7C-40YANDEX-METRIKA-2FVARS-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-28YANDEX-METRIKA-2FVARS-3A-2ACOUNTER-2A-20-28VARIABLE-29-29"></a>

##### [variable](25e9) `yandex-metrika/vars:*counter*` nil

Yandex Metrika's counter id.

<a id="x-28YANDEX-METRIKA-2FVARS-3A-2ATOKEN-2A-20-28VARIABLE-29-29"></a>

##### [variable](6051) `yandex-metrika/vars:*token*` nil

Yandex Metrika's oauth token.

Use this link to get dev token https://oauth.yandex.ru/authorize?response_type=token&client_id=b7e202e6bbd548eb9f0e25dd94eaf6a4 or create your own oauth client application.


[a6d0]: https://40ants.com/yandex-metrika
[63c9]: https://40ants.com/yandex-metrika/#x-28YANDEX-METRIKA-2FLOGS-2FREQUESTS-3ALOG-REQUEST-20CLASS-29
[957b]: https://github.com/40ants/yandex-metrika
[edce]: https://github.com/40ants/yandex-metrika/actions
[d094]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/client.lisp#L1
[d6a4]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/client.lisp#L20
[90ae]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/api.lisp#L1
[7cbf]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/api.lisp#L103
[64c2]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/api.lisp#L124
[b65c]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/api.lisp#L30
[00b5]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/api.lisp#L34
[0d05]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/api.lisp#L83
[ee94]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/download.lisp#L1
[9596]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/download.lisp#L53
[f414]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/download.lisp#L88
[01f9]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L1
[579d]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L104
[37c0]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L113
[d8e1]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L118
[9d96]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L133
[42b3]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L24
[7344]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L38
[f6b2]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L53
[e15b]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L67
[e9e4]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L76
[a894]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L88
[98aa]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/fields.lisp#L98
[a3ff]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L1
[375a]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L151
[3fb2]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L152
[b87e]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L156
[1614]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L160
[59c1]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L164
[9bb9]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L168
[11c6]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L172
[cbcc]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L176
[206d]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L180
[6aa4]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L184
[23b5]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L188
[af11]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L240
[b999]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L248
[82d2]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L282
[8ee7]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L316
[f919]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L344
[2fbf]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L358
[1613]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L368
[09df]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/logs/requests.lisp#L395
[af24]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/vars.lisp#L1
[6051]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/vars.lisp#L12
[25e9]: https://github.com/40ants/yandex-metrika/blob/b872966dd20a499c465478abcec9cc06bc79de7f/src/vars.lisp#L8
[5397]: https://github.com/40ants/yandex-metrika/issues
[8236]: https://quickdocs.org/alexandria
[f8a9]: https://quickdocs.org/cl-interpol
[8347]: https://quickdocs.org/dexador
[6dd8]: https://quickdocs.org/jonathan
[2105]: https://quickdocs.org/lisp-stat
[46a1]: https://quickdocs.org/local-time
[691c]: https://quickdocs.org/log4cl-extras
[cd18]: https://quickdocs.org/secret-values
[c41d]: https://quickdocs.org/serapeum
[ef7f]: https://quickdocs.org/str
[aba2]: https://quickdocs.org/yason

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
