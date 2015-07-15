Token based authentication mechanism using SASL EXTERNAL
========================================================

# Authentication

## Connection diagram
~~~
Making an AMQP connection


+------------------+              +------------------+       +------------------+                   +------------------+
|                  |              |                  |       |                  |                   |                  |
|      Client      |              |  Token SerVice   |       | RabbitMQ SerVer  |                   | Token Validation |
|                  |              |                  |       |                  |                   |      SerVice     |
+--------+---------+              +---------+--------+       +---------+--------+                   +---------+--------+
         |                                  |                          |                                      |
         |                                  |                          |                                      |
         |   +--------------------------+   |                          |                                      |
         +---+                          +-> |                          |                                      |
         |   | Token generation request |   |                          |                                      |
         |   |         SAML 2.0         |   |                          |                                      |
         | <-+                          +---+                          |                                      |
         |   +--------------------------+   |                          |                                      |
         |           Signed token           |                          |                                      |
         |         with permissions         |                          |                                      |
         |                                  |                          |                                      |
         |                                  |                          |                                      |
         |                                  |                          |                                      |
         |                                  |                          |                                      |
         |      AMQP Connection attempt     |                          |                                      |
   +-----------------------------------------------------------------> |                                      |
         |                                  |                          |                                      |
   ^     |                                  |                          |                                      |
   |     |      SASL challenge request      |                          |                                      |
   |     | <-----------------------------------------------------------+                                      |
   |     |                                  |                          |                                      |
   |     |                                  |                          |                                      |
   |     |      SASL challenge response     |                          |                                      |
   +     +-----------------------------------------------------------> |                                      |
  TLS    |           token = $token         |                          |                                      |
   +     |                                  |                          |           HTTP POST /token/          |
   |     |                                  |                          +------------------------------------> |
   |     |                                  |                          |      token = $token, cert = $cert    |
   |     |                                  |                          |                                      |
   |     |                                  |                          |                 200 OK               |
   |     |                                  |                          | <------------------------------------+
   v     |                                  |                          |         username = $username,        |
         |     AMQP connection accepted     |                          |         roles = $roles,              |
   +-----+ <-----------------------------------------------------------+         permissions = $permissions   |
         |                                  |                          |                                      |
         |                                  |                          |                                      |
         |                                  |                          |                                      |
         |                                  |                          |                                      |
         |                                  |                          |                                      |
         |                                  |                          |                                      |
         |                                  |                          |                                      |
         +                                  +                          +                                      +
~~~

## Components

### Token Validation Service
* decrypts signed token with `Secure Token Service` public key
* validates user identity by comparing cert to token subject
* returns user with roles and permissions

Sample request / response:

~~~bash
$ curl -H "Content-Type: application/json" -X POST -d '{"token": "Token", "cert": "Cert"}' http://tvs.cloudcontrolled.com/
{"username":"Guest","roles":["administrator"],"permissions":{"\/":{"configure":".*","write":".*","read":".*"}}}
~~~

## Development & Testing

Check steps you need to start developing and testing [here](README-tests.md).