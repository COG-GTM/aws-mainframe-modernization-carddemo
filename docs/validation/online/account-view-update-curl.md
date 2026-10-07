# Account view / update — curl transcript (docker compose)

Generated against `docker compose up -d --build --wait` (local profile, initial load of `app/data/EBCDIC`, fresh volume) on branch `devin/unt51-18-account-view-update`; token redacted. Steps: sign-on, view, 404, edit error, ENTER (`confirm=false`), PF5 (`confirm=true`), stale versions → 409, re-read, no-change.

```text
### Sign on as USER0001 (type U)
$ curl http://localhost:8091/api/v1/auth/login -H Content-Type: application/json -d {"userId":"USER0001","password":"PASSWORD"} 
HTTP 200
{
  "token": "<redacted>",
  "tokenType": "Bearer",
  "expiresAt": "2026-10-07T12:22:22.722918370Z",
  "userId": "USER0001",
  "role": "USER",
  "userType": "U",
  "targetMenu": "COMEN01C",
  "targetMenuUrl": "/api/v1/menu/main",
  "navigation": {
    "fromTranId": "CC00",
    "fromProgram": "COSGN00C",
    "toTranId": "CM00",
    "toProgram": "COMEN01C",
    "pgmContext": "ENTER",
    "custId": null,
    "acctId": null,
    "cardNum": null
  }
}

### View account 1 (COACTVWC)
$ curl http://localhost:8091/api/v1/accounts/00000000001 -H Authorization: Bearer $TOKEN 
HTTP 200
{
  "header": {
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CAVW",
    "programName": "COACTVWC",
    "currentDate": "10/07/26",
    "currentTime": "11:22:22",
    "applId": "CARDDEMO",
    "sysId": "CDMO"
  },
  "infoMessage": "Enter or update id of account to display",
  "message": "",
  "acctId": "00000000001",
  "activeStatus": "Y",
  "openDate": "2014-11-20",
  "expirationDate": "2025-05-20",
  "reissueDate": "2025-05-20",
  "creditLimit": 2020.00,
  "cashCreditLimit": 1020.00,
  "currentBalance": 194.00,
  "currentCycleCredit": 0.00,
  "currentCycleDebit": 0.00,
  "groupId": "",
  "custId": "000000001",
  "ssn": "020-97-3888",
  "ficoScore": "274",
  "dateOfBirth": "1961-06-08",
  "firstName": "Immanuel",
  "middleName": "Madeline",
  "lastName": "Kessler",
  "addressLine1": "618 Deshaun Route",
  "addressLine2": "Apt. 802",
  "city": "Altenwerthshire",
  "state": "NC",
  "zip": "12546",
  "country": "USA",
  "phone1": "(908)119-8310",
  "phone2": "(373)693-8684",
  "governmentId": "00000000000049368437",
  "eftAccountId": "0053581756",
  "primaryCardHolder": "Y",
  "cardNum": "9680294154603697",
  "cardNumbers": [
    "9680294154603697"
  ],
  "accountVersion": 0,
  "customerVersion": 0,
  "updateForm": {
    "accountVersion": 0,
    "customerVersion": 0,
    "confirm": false,
    "activeStatus": "Y",
    "openDate": {
      "year": "2014",
      "month": "11",
      "day": "20"
    },
    "creditLimit": "2020.00",
    "expiryDate": {
      "year": "2025",
      "month": "05",
      "day": "20"
    },
    "cashCreditLimit": "1020.00",
    "reissueDate": {
      "year": "2025",
      "month": "05",
      "day": "20"
    },
    "currentBalance": "194.00",
    "currentCycleCredit": "0.00",
    "currentCycleDebit": "0.00",
    "groupId": "",
    "ssn": {
      "part1": "020",
      "part2": "97",
      "part3": "3888"
    },
    "dateOfBirth": {
      "year": "1961",
      "month": "06",
      "day": "08"
    },
    "ficoScore": "274",
    "firstName": "Immanuel",
    "middleName": "Madeline",
    "lastName": "Kessler",
    "addressLine1": "618 Deshaun Route",
    "addressLine2": "Apt. 802",
    "state": "NC",
    "zip": "12546",
    "city": "Altenwerthshire",
    "phone1": {
      "areaCode": "908",
      "prefix": "119",
      "lineNumber": "8310"
    },
    "phone2": {
      "areaCode": "373",
      "prefix": "693",
      "lineNumber": "8684"
    },
    "governmentId": "00000000000049368437",
    "eftAccountId": "0053581756",
    "primaryCardHolder": "Y"
  },
  "exit": {
    "fromTranId": "CAVW",
    "fromProgram": "COACTVWC",
    "toTranId": "CM00",
    "toProgram": "COMEN01C",
    "pgmContext": "ENTER",
    "custId": null,
    "acctId": null,
    "cardNum": null
  }
}

### Unknown account -> 404 NOTFND (CXACAIX)
$ curl http://localhost:8091/api/v1/accounts/99999999999 -H Authorization: Bearer $TOKEN 
HTTP 404
{
  "type": "about:blank",
  "title": "Not Found",
  "status": 404,
  "detail": "Account:99999999999 not found in Cross ref file.  Resp:000000013  Reas:0000",
  "instance": "/api/v1/accounts/99999999999",
  "code": "NOTFND",
  "field": null,
  "message": "Account:99999999999 not found in Cross ref file.  Resp:000000013  Reas:0000",
  "cicsResp": "NOTFND"
}

### Update with the sample FICO 274 left as is -> 400 (first COBOL edit error + every red field)
$ curl -X PUT http://localhost:8091/api/v1/accounts/00000000001 -H Authorization: Bearer $TOKEN -H Content-Type: application/json -d {"accountVersion":0,"customerVersion":0,"confirm":false,"activeStatus":"Y","openDate":{"year":"2014","month":"11","day":"20"},"creditLimit":"2020.00","expiryDate":{"year":"2025","month":"05","day":"20"},"cashCreditLimit":"1020.00","reissueDate":{"year":"2025","month":"05","day":"20"},"currentBalance":"194.00","currentCycleCredit":"0.00","currentCycleDebit":"0.00","groupId":"","ssn":{"part1":"020","part2":"97","part3":"3888"},"dateOfBirth":{"year":"1961","month":"06","day":"08"},"ficoScore":"274","firstName":"Emmanuel","middleName":"Madeline","lastName":"Kessler","addressLine1":"618 Deshaun Route","addressLine2":"Apt. 802","state":"NC","zip":"12546","city":"Altenwerthshire","phone1":{"areaCode":"908","prefix":"119","lineNumber":"8310"},"phone2":{"areaCode":"373","prefix":"693","lineNumber":"8684"},"governmentId":"00000000000049368437","eftAccountId":"0053581756","primaryCardHolder":"Y"} 
HTTP 400
{
  "type": "about:blank",
  "title": "Bad Request",
  "status": 400,
  "detail": "FICO Score: should be between 300 and 850",
  "instance": "/api/v1/accounts/00000000001",
  "code": "INVREQ",
  "field": "ficoScore",
  "message": "FICO Score: should be between 300 and 850",
  "cicsResp": "INVREQ",
  "invalidFields": [
    "ficoScore",
    "phone2.areaCode",
    "zip",
    "state"
  ]
}

### ENTER: confirm=false -> VALIDATED, nothing written
$ curl -X PUT http://localhost:8091/api/v1/accounts/00000000001 -H Authorization: Bearer $TOKEN -H Content-Type: application/json -d {"accountVersion":0,"customerVersion":0,"confirm":false,"activeStatus":"Y","openDate":{"year":"2014","month":"11","day":"20"},"creditLimit":"$21,000.00","expiryDate":{"year":"2025","month":"05","day":"20"},"cashCreditLimit":"1020.00","reissueDate":{"year":"2025","month":"05","day":"20"},"currentBalance":"194.00","currentCycleCredit":"0.00","currentCycleDebit":"0.00","groupId":"","ssn":{"part1":"020","part2":"97","part3":"3888"},"dateOfBirth":{"year":"1961","month":"06","day":"08"},"ficoScore":"704","firstName":"Emmanuel","middleName":"Madeline","lastName":"Kessler","addressLine1":"618 Deshaun Route","addressLine2":"Apt. 802","state":"NC","zip":"27601","city":"Altenwerthshire","phone1":{"areaCode":"908","prefix":"119","lineNumber":"8310"},"phone2":{"areaCode":"212","prefix":"693","lineNumber":"8684"},"governmentId":"00000000000049368437","eftAccountId":"0053581756","primaryCardHolder":"Y"} 
HTTP 200
{
  "header": {
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CAUP",
    "programName": "COACTUPC",
    "currentDate": "10/07/26",
    "currentTime": "11:22:23",
    "applId": "CARDDEMO",
    "sysId": "CDMO"
  },
  "state": "VALIDATED",
  "updated": false,
  "infoMessage": "Changes validated.Press F5 to save",
  "message": "",
  "account": {
    "header": {
      "title01": "AWS Mainframe Modernization",
      "title02": "CardDemo",
      "tranId": "CAUP",
      "programName": "COACTUPC",
      "currentDate": "10/07/26",
      "currentTime": "11:22:23",
      "applId": "CARDDEMO",
      "sysId": "CDMO"
    },
    "infoMessage": "Changes validated.Press F5 to save",
    "message": "",
    "acctId": "00000000001",
    "activeStatus": "Y",
    "openDate": "2014-11-20",
    "expirationDate": "2025-05-20",
    "reissueDate": "2025-05-20",
    "creditLimit": 2020.00,
    "cashCreditLimit": 1020.00,
    "currentBalance": 194.00,
    "currentCycleCredit": 0.00,
    "currentCycleDebit": 0.00,
    "groupId": "",
    "custId": "000000001",
    "ssn": "020-97-3888",
    "ficoScore": "274",
    "dateOfBirth": "1961-06-08",
    "firstName": "Immanuel",
    "middleName": "Madeline",
    "lastName": "Kessler",
    "addressLine1": "618 Deshaun Route",
    "addressLine2": "Apt. 802",
    "city": "Altenwerthshire",
    "state": "NC",
    "zip": "12546",
    "country": "USA",
    "phone1": "(908)119-8310",
    "phone2": "(373)693-8684",
    "governmentId": "00000000000049368437",
    "eftAccountId": "0053581756",
    "primaryCardHolder": "Y",
    "cardNum": "9680294154603697",
    "cardNumbers": [
      "9680294154603697"
    ],
    "accountVersion": 0,
    "customerVersion": 0,
    "updateForm": {
      "accountVersion": 0,
      "customerVersion": 0,
      "confirm": false,
      "activeStatus": "Y",
      "openDate": {
        "year": "2014",
        "month": "11",
        "day": "20"
      },
      "creditLimit": "2020.00",
      "expiryDate": {
        "year": "2025",
        "month": "05",
        "day": "20"
      },
      "cashCreditLimit": "1020.00",
      "reissueDate": {
        "year": "2025",
        "month": "05",
        "day": "20"
      },
      "currentBalance": "194.00",
      "currentCycleCredit": "0.00",
      "currentCycleDebit": "0.00",
      "groupId": "",
      "ssn": {
        "part1": "020",
        "part2": "97",
        "part3": "3888"
      },
      "dateOfBirth": {
        "year": "1961",
        "month": "06",
        "day": "08"
      },
      "ficoScore": "274",
      "firstName": "Immanuel",
      "middleName": "Madeline",
      "lastName": "Kessler",
      "addressLine1": "618 Deshaun Route",
      "addressLine2": "Apt. 802",
      "state": "NC",
      "zip": "12546",
      "city": "Altenwerthshire",
      "phone1": {
        "areaCode": "908",
        "prefix": "119",
        "lineNumber": "8310"
      },
      "phone2": {
        "areaCode": "373",
        "prefix": "693",
        "lineNumber": "8684"
      },
      "governmentId": "00000000000049368437",
      "eftAccountId": "0053581756",
      "primaryCardHolder": "Y"
    },
    "exit": {
      "fromTranId": "CAUP",
      "fromProgram": "COACTUPC",
      "toTranId": "CM00",
      "toProgram": "COMEN01C",
      "pgmContext": "ENTER",
      "custId": null,
      "acctId": null,
      "cardNum": null
    }
  }
}

### PF5: confirm=true -> COMMITTED (account + customer in one transaction)
$ curl -X PUT http://localhost:8091/api/v1/accounts/00000000001 -H Authorization: Bearer $TOKEN -H Content-Type: application/json -d {"accountVersion":0,"customerVersion":0,"confirm":true,"activeStatus":"Y","openDate":{"year":"2014","month":"11","day":"20"},"creditLimit":"$21,000.00","expiryDate":{"year":"2025","month":"05","day":"20"},"cashCreditLimit":"1020.00","reissueDate":{"year":"2025","month":"05","day":"20"},"currentBalance":"194.00","currentCycleCredit":"0.00","currentCycleDebit":"0.00","groupId":"","ssn":{"part1":"020","part2":"97","part3":"3888"},"dateOfBirth":{"year":"1961","month":"06","day":"08"},"ficoScore":"704","firstName":"Emmanuel","middleName":"Madeline","lastName":"Kessler","addressLine1":"618 Deshaun Route","addressLine2":"Apt. 802","state":"NC","zip":"27601","city":"Altenwerthshire","phone1":{"areaCode":"908","prefix":"119","lineNumber":"8310"},"phone2":{"areaCode":"212","prefix":"693","lineNumber":"8684"},"governmentId":"00000000000049368437","eftAccountId":"0053581756","primaryCardHolder":"Y"} 
HTTP 200
{
  "header": {
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CAUP",
    "programName": "COACTUPC",
    "currentDate": "10/07/26",
    "currentTime": "11:22:23",
    "applId": "CARDDEMO",
    "sysId": "CDMO"
  },
  "state": "COMMITTED",
  "updated": true,
  "infoMessage": "Changes committed to database",
  "message": "",
  "account": {
    "header": {
      "title01": "AWS Mainframe Modernization",
      "title02": "CardDemo",
      "tranId": "CAUP",
      "programName": "COACTUPC",
      "currentDate": "10/07/26",
      "currentTime": "11:22:23",
      "applId": "CARDDEMO",
      "sysId": "CDMO"
    },
    "infoMessage": "Changes committed to database",
    "message": "",
    "acctId": "00000000001",
    "activeStatus": "Y",
    "openDate": "2014-11-20",
    "expirationDate": "2025-05-20",
    "reissueDate": "2025-05-20",
    "creditLimit": 21000.00,
    "cashCreditLimit": 1020.00,
    "currentBalance": 194.00,
    "currentCycleCredit": 0.00,
    "currentCycleDebit": 0.00,
    "groupId": "",
    "custId": "000000001",
    "ssn": "020-97-3888",
    "ficoScore": "704",
    "dateOfBirth": "1961-06-08",
    "firstName": "Emmanuel",
    "middleName": "Madeline",
    "lastName": "Kessler",
    "addressLine1": "618 Deshaun Route",
    "addressLine2": "Apt. 802",
    "city": "Altenwerthshire",
    "state": "NC",
    "zip": "27601",
    "country": "USA",
    "phone1": "(908)119-8310",
    "phone2": "(212)693-8684",
    "governmentId": "00000000000049368437",
    "eftAccountId": "0053581756",
    "primaryCardHolder": "Y",
    "cardNum": "9680294154603697",
    "cardNumbers": [
      "9680294154603697"
    ],
    "accountVersion": 1,
    "customerVersion": 1,
    "updateForm": {
      "accountVersion": 1,
      "customerVersion": 1,
      "confirm": false,
      "activeStatus": "Y",
      "openDate": {
        "year": "2014",
        "month": "11",
        "day": "20"
      },
      "creditLimit": "21000.00",
      "expiryDate": {
        "year": "2025",
        "month": "05",
        "day": "20"
      },
      "cashCreditLimit": "1020.00",
      "reissueDate": {
        "year": "2025",
        "month": "05",
        "day": "20"
      },
      "currentBalance": "194.00",
      "currentCycleCredit": "0.00",
      "currentCycleDebit": "0.00",
      "groupId": "",
      "ssn": {
        "part1": "020",
        "part2": "97",
        "part3": "3888"
      },
      "dateOfBirth": {
        "year": "1961",
        "month": "06",
        "day": "08"
      },
      "ficoScore": "704",
      "firstName": "Emmanuel",
      "middleName": "Madeline",
      "lastName": "Kessler",
      "addressLine1": "618 Deshaun Route",
      "addressLine2": "Apt. 802",
      "state": "NC",
      "zip": "27601",
      "city": "Altenwerthshire",
      "phone1": {
        "areaCode": "908",
        "prefix": "119",
        "lineNumber": "8310"
      },
      "phone2": {
        "areaCode": "212",
        "prefix": "693",
        "lineNumber": "8684"
      },
      "governmentId": "00000000000049368437",
      "eftAccountId": "0053581756",
      "primaryCardHolder": "Y"
    },
    "exit": {
      "fromTranId": "CAUP",
      "fromProgram": "COACTUPC",
      "toTranId": "CM00",
      "toProgram": "COMEN01C",
      "pgmContext": "ENTER",
      "custId": null,
      "acctId": null,
      "cardNum": null
    }
  }
}

### Same request again with the versions read before the commit -> 409 CHANGED
$ curl -X PUT http://localhost:8091/api/v1/accounts/00000000001 -H Authorization: Bearer $TOKEN -H Content-Type: application/json -d {"accountVersion":0,"customerVersion":0,"confirm":true,"activeStatus":"Y","openDate":{"year":"2014","month":"11","day":"20"},"creditLimit":"$21,000.00","expiryDate":{"year":"2025","month":"05","day":"20"},"cashCreditLimit":"1020.00","reissueDate":{"year":"2025","month":"05","day":"20"},"currentBalance":"194.00","currentCycleCredit":"0.00","currentCycleDebit":"0.00","groupId":"","ssn":{"part1":"020","part2":"97","part3":"3888"},"dateOfBirth":{"year":"1961","month":"06","day":"08"},"ficoScore":"704","firstName":"Emmanuel","middleName":"Madeline","lastName":"Other","addressLine1":"618 Deshaun Route","addressLine2":"Apt. 802","state":"NC","zip":"27601","city":"Altenwerthshire","phone1":{"areaCode":"908","prefix":"119","lineNumber":"8310"},"phone2":{"areaCode":"212","prefix":"693","lineNumber":"8684"},"governmentId":"00000000000049368437","eftAccountId":"0053581756","primaryCardHolder":"Y"} 
HTTP 409
{
  "type": "about:blank",
  "title": "Conflict",
  "status": 409,
  "detail": "Record changed by some one else. Please review",
  "instance": "/api/v1/accounts/00000000001",
  "code": "CHANGED",
  "field": null,
  "message": "Record changed by some one else. Please review",
  "cicsResp": "CHANGED"
}

### Re-read: new values and versions
{
  "firstName": "Emmanuel",
  "creditLimit": 21000.00,
  "ficoScore": "704",
  "zip": "27601",
  "phone2": "(212)693-8684",
  "accountVersion": 1,
  "customerVersion": 1
}

### Fresh form, nothing changed -> SHOW + no-change message
$ curl -X PUT http://localhost:8091/api/v1/accounts/00000000001 -H Authorization: Bearer $TOKEN -H Content-Type: application/json -d {"accountVersion":1,"customerVersion":1,"confirm":false,"activeStatus":"Y","openDate":{"year":"2014","month":"11","day":"20"},"creditLimit":"21000.00","expiryDate":{"year":"2025","month":"05","day":"20"},"cashCreditLimit":"1020.00","reissueDate":{"year":"2025","month":"05","day":"20"},"currentBalance":"194.00","currentCycleCredit":"0.00","currentCycleDebit":"0.00","groupId":"","ssn":{"part1":"020","part2":"97","part3":"3888"},"dateOfBirth":{"year":"1961","month":"06","day":"08"},"ficoScore":"704","firstName":"Emmanuel","middleName":"Madeline","lastName":"Kessler","addressLine1":"618 Deshaun Route","addressLine2":"Apt. 802","state":"NC","zip":"27601","city":"Altenwerthshire","phone1":{"areaCode":"908","prefix":"119","lineNumber":"8310"},"phone2":{"areaCode":"212","prefix":"693","lineNumber":"8684"},"governmentId":"00000000000049368437","eftAccountId":"0053581756","primaryCardHolder":"Y"} 
HTTP 200
{
  "header": {
    "title01": "AWS Mainframe Modernization",
    "title02": "CardDemo",
    "tranId": "CAUP",
    "programName": "COACTUPC",
    "currentDate": "10/07/26",
    "currentTime": "11:22:23",
    "applId": "CARDDEMO",
    "sysId": "CDMO"
  },
  "state": "SHOW",
  "updated": false,
  "infoMessage": "Update account details presented above.",
  "message": "No change detected with respect to values fetched.",
  "account": {
    "header": {
      "title01": "AWS Mainframe Modernization",
      "title02": "CardDemo",
      "tranId": "CAUP",
      "programName": "COACTUPC",
      "currentDate": "10/07/26",
      "currentTime": "11:22:23",
      "applId": "CARDDEMO",
      "sysId": "CDMO"
    },
    "infoMessage": "Update account details presented above.",
    "message": "No change detected with respect to values fetched.",
    "acctId": "00000000001",
    "activeStatus": "Y",
    "openDate": "2014-11-20",
    "expirationDate": "2025-05-20",
    "reissueDate": "2025-05-20",
    "creditLimit": 21000.00,
    "cashCreditLimit": 1020.00,
    "currentBalance": 194.00,
    "currentCycleCredit": 0.00,
    "currentCycleDebit": 0.00,
    "groupId": "",
    "custId": "000000001",
    "ssn": "020-97-3888",
    "ficoScore": "704",
    "dateOfBirth": "1961-06-08",
    "firstName": "Emmanuel",
    "middleName": "Madeline",
    "lastName": "Kessler",
    "addressLine1": "618 Deshaun Route",
    "addressLine2": "Apt. 802",
    "city": "Altenwerthshire",
    "state": "NC",
    "zip": "27601",
    "country": "USA",
    "phone1": "(908)119-8310",
    "phone2": "(212)693-8684",
    "governmentId": "00000000000049368437",
    "eftAccountId": "0053581756",
    "primaryCardHolder": "Y",
    "cardNum": "9680294154603697",
    "cardNumbers": [
      "9680294154603697"
    ],
    "accountVersion": 1,
    "customerVersion": 1,
    "updateForm": {
      "accountVersion": 1,
      "customerVersion": 1,
      "confirm": false,
      "activeStatus": "Y",
      "openDate": {
        "year": "2014",
        "month": "11",
        "day": "20"
      },
      "creditLimit": "21000.00",
      "expiryDate": {
        "year": "2025",
        "month": "05",
        "day": "20"
      },
      "cashCreditLimit": "1020.00",
      "reissueDate": {
        "year": "2025",
        "month": "05",
        "day": "20"
      },
      "currentBalance": "194.00",
      "currentCycleCredit": "0.00",
      "currentCycleDebit": "0.00",
      "groupId": "",
      "ssn": {
        "part1": "020",
        "part2": "97",
        "part3": "3888"
      },
      "dateOfBirth": {
        "year": "1961",
        "month": "06",
        "day": "08"
      },
      "ficoScore": "704",
      "firstName": "Emmanuel",
      "middleName": "Madeline",
      "lastName": "Kessler",
      "addressLine1": "618 Deshaun Route",
      "addressLine2": "Apt. 802",
      "state": "NC",
      "zip": "27601",
      "city": "Altenwerthshire",
      "phone1": {
        "areaCode": "908",
        "prefix": "119",
        "lineNumber": "8310"
      },
      "phone2": {
        "areaCode": "212",
        "prefix": "693",
        "lineNumber": "8684"
      },
      "governmentId": "00000000000049368437",
      "eftAccountId": "0053581756",
      "primaryCardHolder": "Y"
    },
    "exit": {
      "fromTranId": "CAUP",
      "fromProgram": "COACTUPC",
      "toTranId": "CM00",
      "toProgram": "COMEN01C",
      "pgmContext": "ENTER",
      "custId": null,
      "acctId": null,
      "cardNum": null
    }
  }
}

```
