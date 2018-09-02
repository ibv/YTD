(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2009-12 Pepak
http://www.pepak.net/ytd                                  http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2009-12 Pepak (http://www.pepak.net)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Pepak nor the
      names of his contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL PEPAK BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

******************************************************************************)

unit uHttp;
{$INCLUDE 'ytd.inc'}

interface

uses
  blcksock;
  
{gnugettext: scan-all}
const
  SOCKSTATUS_RESOLVING_BEGAN = 'Resolving began'; // HTTP socket status: resolving began
  SOCKSTATUS_RESOLVING_ENDED = 'Resolving ended'; // HTTP socket status: resolving ended
  SOCKSTATUS_SOCKET_CREATED = 'Socket created'; // HTTP socket status: socket created
  SOCKSTATUS_SOCKET_CLOSED = 'Socket closed'; // HTTP socket status: socket closed
  SOCKSTATUS_BOUND_TO_IP_PORT = 'Bound to IP/port'; // HTTP socket status: bound to IP address and port
  SOCKSTATUS_CONNECTED = 'Connected'; // HTTP socket status: connected
  SOCKSTATUS_CAN_READ_DATA = 'Can read data'; // HTTP socket status: can read data
  SOCKSTATUS_CAN_WRITE_DATA = 'Can write data'; // HTTP socket status: can write data
  SOCKSTATUS_LISTENING = 'Listening'; // HTTP socket status: listening for connections
  SOCKSTATUS_ACCEPTED_CONNECTION = 'Accepted connection'; // HTTP socket status: connection accepted
  SOCKSTATUS_READ_DATA = 'Read data'; // HTTP socket status: data was read
  SOCKSTATUS_WROTE_DATA = 'Wrote data'; // HTTP socket status: data was written
  SOCKSTATUS_WAITING = 'Waiting'; // HTTP socket status: data is waiting
  SOCKSTATUS_SOCKET_ERROR = 'Socket error'; // HTTP socket status: socket error
{gnugettext: reset}

const SockStatusReasons: array[THookSocketReason] of string
              = (SOCKSTATUS_RESOLVING_BEGAN, SOCKSTATUS_RESOLVING_ENDED, SOCKSTATUS_SOCKET_CREATED, SOCKSTATUS_SOCKET_CLOSED, SOCKSTATUS_BOUND_TO_IP_PORT, SOCKSTATUS_CONNECTED,
                 SOCKSTATUS_CAN_READ_DATA, SOCKSTATUS_CAN_WRITE_DATA, SOCKSTATUS_LISTENING, SOCKSTATUS_ACCEPTED_CONNECTION, SOCKSTATUS_READ_DATA, SOCKSTATUS_WROTE_DATA,
                 SOCKSTATUS_WAITING, SOCKSTATUS_SOCKET_ERROR);

implementation

end.
