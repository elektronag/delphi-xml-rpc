{
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/source/xmlrpcmime.pas,v 1.1.1.1 2003-11-19 22:11:55 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}

{ Copyright Ralf Junker <ralfjunker@gmx.de> 2000-2001
  http://www.zeitungsjunge.de/delphi/ }

unit xmlrpcmime;
{
Unit:           rjMime
Version:        1.50
Last Modified:  1. February 2001
Author:         Ralf Junker <ralfjunker@gmx.de>
Internet:       http://www.zeitungsjunge.de/delphi/

Description:    Ligtening fast Mime (Base64) Encoding and Decoding routines.
                More detailed descriptions follow the declarations of the
                procedures and functions below.

Legal:          This software is provided 'as-is', without any express or
                implied warranty. In no event will the author be held liable
                for any  damages arising from the use of this software.

                Permission is granted to anyone to use this software for any
                purpose, including commercial applications, and to alter it
                and redistribute it freely, subject to the following
                restrictions:

                1. The origin of this software must not be misrepresented,
                    you must not claim that you wrote the original software.
                    If you use this software in a product, an acknowledgment
                    in the product documentation would be appreciated but is
                    not required.

                2. Altered source versions must be plainly marked as such, and
                   must not be misrepresented as being the original software.

                3. This notice may not be removed or altered from any source
                   distribution.

History:

Version 1.50
------------
01.02.2000     Added support for line breaks (CRLF) during Mime encoding
               as required by RFC 1521. Since inserting line breaks is the
               default in RFC 1521, I changed the standard encoding functions
               to encode WITH line breaks. This may require changes to your
               code: Encoding without inserting line breaks is still provided
               by the ...NoCRLF procedures. There are now two sets of Mime
               encoding procedures and functions:

               Encoding WITH line breaks       | Encoding WITHOUT line breaks
               ("strict" RFC 1521)             | ("loose" RFC 1521)
               --------------------------------|--------------------------------
               MimeEncode                      | MimeEncodeNoCRLF
               MimeEncodeString                | MimeEncodeStringNoCRLF
               MimeEncodeStream                | MimeEncodeStreamNoCRLF
               MimeEncodedSize                 | MimeEncodedSizeNoCRLF

               If you migrate from rjMime 1.31 to rjMime 1.50, you might need to
               adjust your code depending on the required results. If you
               don't want to change any behaviour as compared to earlier
               versions of rjMime, simply add "NoCRLF" to all calls to Mime
               encoding functions.

               Please note that there is no MimeDecodeNoCRLF equivalent since
               the decoding routines skip line breaks and white space anyway.

               New DecodeHttpBasicAuthentication procedure according to
               RFC 1945. See description below for deatils.

               Version 1.50 also fixes a critical bug in MimeDecode
               (added missing @). This bug did not affect any of the other
               functions.

Version 1.31
------------
20.11.2000      Defined the OutputBuffer parameters as untyped "out"
                in the core encoding / decoding routines. This does not
                affect much but consistency in clarity.

Version 1.30
------------
17.11.2000      Changed the interface part to the core encoding and decoding
                routines from Pointer to Untyped. This way they no longer
                have to check if the pointers are not nil.

                Replaced all Integer types with Cardinal types
                where the sign was not needed (which was in all cases).

                Thanks to Robert Marqwart <robert_marquardt@gmx.de>
                for pointing these issues out.

Version 1.20
------------
20.06.2000      Bugfix for MimeEncodeStream: Wrong BUFFER_SIZE resulted in
                additional bytes stuffed inbetween the OutBuffer.

                Changed the order of the variables in the MimeEncode interface
                (moved InputBytesCount from last to 2nd, just after InputBuffer).
                Sorry for the inconvenience, but this way it is more consistent
                with MimeDecode. Now MimeEncode has the following variable order:

                procedure MimeEncode (
                  const InputBuffer: Pointer;
                  const InputBytesCount: Integer;
                  const OutputBuffer: Pointer);

                MimeDecode: Interface chage to make it a function: It now returns
                the number of bytes written to the output buffer not as a var
                but as the result of a function. This way coding is made
                simpler since not OutputCount variable needs to be defined.

                Introduced two new functions mostly for internal use:

                * MimeDecodePartial: This is necessary to decode large
                  blocks of data in multiple parts. On initialization,
                  call MimeDecodePartial with ByteBuffer := 0 and
                  ByteBufferSpace := 4. Then repeatedly call it again
                  with new data loaded into the buffer. At the end of all data,
                * MimeDecodePartialEnd writes the remaining bytes from ByteBuffer
                  to the outputbuffer (see MimeDecodeStream for an example).

                MimeDecodePartial_ are necessary to decode inconsitent data
                (with linebreaks, tabs, whitespace) in multiple parts
                like in MimeDecodeStream.

Version 1.10
------------
19.04.2000      Fixed a small bug in MimeEncode which sometimes screwed up
                the very first bytes of the encoded output.

                Added the following wrapper functions:
                * MimeEncodeString & MimeDecodeString
                * MimeEncodeStream & MimeDecodeStream

Version 1.01
------------
09.04.2000      Fixed a bug in MIME_DECODE_TABLE which caused wrong results
                decoding binary files.

Version 1.00
------------
17.01.2000      Initial Public Release

Copyright (c) 2000 Ralf Junker
}

interface

uses
 Classes;

function MimeEncodeString (const s: AnsiString): AnsiString;
{ MimeEncodeString takes a string, encodes it, and returns the result as a string.
  To decode the result string, use MimeDecodeString. }

function MimeEncodeStringNoCRLF (const s: AnsiString): AnsiString;
{ MimeEncodeStringNoCRLF is just like MimeEncodeString, but does NOT insert line breaks. }

function MimeDecodeString (const s: AnsiString): AnsiString;
{ MimeDecodeString takes a a string, decodes it, and returns the result as a string.
Use MimeDecodeString to decode a string previously encoded with MimeEncodeString. }

procedure MimeEncodeStream (const InputStream: TStream; const OutputStream: TStream);
{ MimeEncodeStream encodes InputStream WITH inserting line breaks.
  Encoding starts at the InputStream's current position and continues until the end.
  Decoded output is written to OutputStream, again starting at the current position.
  When done, the function will not reset either stream's positions,
  but leave InputStream at the last read position (i.e. the end) and
  OutputStream at the last write position (which can, but most not be the end).
  To encode the entire InputStream from beginning to end, make sure
  that its offset is positioned at the beginning of the stream. You can
  force this by issuing

       InputStream.Seek (0, soFromBeginning);

  before calling this function. }

procedure MimeEncodeStreamNoCRLF (const InputStream: TStream; const OutputStream: TStream);
{ MimeEncodeStreamNoCRLF is just like MimeEncodeStream, but does NOT insert line breaks. }

procedure MimeDecodeStream (const InputStream: TStream; const OutputStream: TStream);
{ MimeDecodeStream decodes InputStream starting at the current position
  up to the end and writes the result to OutputStream, again starting at
  the current position. When done, it will not reset either stream's positions,
  but leave InputStream at the last read position (i.e. the end) and
  OutputStream at the last write position (which can, but most not be the end).
  To decode the entire InputStream from beginning to end, make sure
  that its offset is positioned at the beginning of the stream. You can
  force this by issuing Seek (0, soFromBeginning) before calling this function. }

function MimeEncodedSize (const i: Cardinal): Cardinal;
{ Calculates the output size of i MimeEncoded bytes, i.e. the memory required
  for all decoded data plus the line breaks. Use for MimeEncode only. }

function MimeEncodedSizeNoCRLF (const i: Cardinal): Cardinal;
{ Calculates the output size of i MimeEncodedNoCRLF bytes, i.e. the memory
  required for all decoded data. Use for MimeEncodedNoCRLF only. }

function MimeDecodedSize (const i: Cardinal): Cardinal;
{ Calculates the maximum output size of i MimeDecoded bytes.
  You may use it for MimeDecode to calculate the maximum amount of memory
  required for decoding in one single pass. }

procedure DecodeHttpBasicAuthentication (const BasicCredentials: AnsiString; out UserId, PassWord: AnsiString);
{ Decodes the UserID and Password for HTTP Basic Authentication. Pass the
  contents of the Authorization Header as BasicCredentials and DecodeHttpBasicAuthentication
  will return the unencoded UserID and Password. If either of the two can not be
  decoded or found, they will result in an empty string (''). This procedure is
  inspired by Shiv.

  The following quote from "Request for Comments (RFC) 1945: Hypertext Transfer
  Protocol -- HTTP/1.0" has the details:

  11.1  Basic Authentication Scheme

   The "basic" authentication scheme is based on the model that the user
   agent must authenticate itself with a user-ID and a password for each
   realm. The realm value should be considered an opaque string which
   can only be compared for equality with other realms on that server.
   The server will authorize the request only if it can validate the
   user-ID and password for the protection space of the Request-URI.
   There are no optional authentication parameters.

   Upon receipt of an unauthorized request for a URI within the
   protection space, the server should respond with a challenge like the
   following:

       WWW-Authenticate: Basic realm="WallyWorld"

   where "WallyWorld" is the string assigned by the server to identify
   the protection space of the Request-URI.

   To receive authorization, the client sends the user-ID and password,
   separated by a single colon (":") character, within a base64 [5]
   encoded string in the credentials.

       basic-credentials = "Basic" SP basic-cookie

       basic-cookie      = <base64 [5] encoding of userid-password,
                            except not limited to 76 char/line>

       userid-password   = [ token ] ":" *TEXT

   If the user agent wishes to send the user-ID "Aladdin" and password
   "open sesame", it would use the following header field:

       Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==

   The basic authentication scheme is a non-secure method of filtering
   unauthorized access to resources on an HTTP server. It is based on
   the assumption that the connection between the client and the server
   can be regarded as a trusted carrier. As this is not generally true
   on an open network, the basic authentication scheme should be used
   accordingly. In spite of this, clients should implement the scheme in
   order to communicate with servers that use it. }

procedure MimeEncode (const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
{ MimeEncode is the primary Mime encoding routine.
  Line breaks will be inserted after each full line.

  CAUTTION: OutputBuffer must have enough memory allocated to take all encoded output.
  MimeEncodedSize (InputBytesCount) calculates this amount in bytes. MimeEncode will
  then fill the entire OutputBuffer, so there is no OutputBytesCount result for
  this procedure. Preallocating all memory at once (as required by MimeEncode)
  avoids the time-cosuming process of reallocation.

  If not all data fits into memory at once, you can NOT use MimeEncode multiple times.
  Instead, use a combintion of MimeEncodeFullLines and MimeEncodeNoCRLF. }

procedure MimeEncodeNoCRLF (const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
{ MimeEncodeNoCRLF is just like MimeEncode, but does NOT insert line breaks.

  Unlike MimeEncode, you can use MimeEncodeNoCRLF multiple times if not all data
  fits into memory at once. But you must be very careful about the size
  of the InputBuffer. See comments on BUFFER_SIZE below for details
  and MimeEncodeStreamNoCRLF for an example. }

procedure MimeEncodeFullLines (const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
{ MimeEncodeFullLines will decode full lines of MIME_ENCODED_LINE_BREAK length.
  A line break (CRLF) will be inserted after each line including the last one.
  Any remaining input which would not result in a full line will not be encoded.
  To encode the remaining partial line, use MimeEncodeNoCRLF with the appropriate parameters.
  MimeEncodeFullLines requires an OutputBuffer large enough for all encoded output.
  The required size of the OutputBuffer can be calculated with

      (InputByteCount + 2) div 3 * 4 + InputByteCount div MIME_DECODED_LINE_BREAK * 2

  MimeEncodeFullLines will fill the entire OutputBuffer of that size. }

function MimeDecode (const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer): Cardinal;
{ The primary Mime decoding routines. MimeDecode works with all MimeEncoded data,
  no matter if it was encoded with or without line breaks. Line breaks characters
  are outside of the base64 alphabet and will be ignored.

  CAUTION: OutputBuffer must have enough memory allocated to take all output.
  MimeDecodedSize (InputBytesCount) calculates this amount in bytes. There is
  no guarantee that all output will be filled after decoding. All decoding
  functions therefore return the acutal number of bytes written to OutputBuffer.
  Preallocating all memory at once (as is required by MimeDecode)
  avoids the time-cosuming process of reallocation. After calling
  MimeDecode, simply cut the allocated memory down to OutputBytesCount,
  i.e. SetLength (OutString, OutputBytesCount).

  If not all data fits into memory at once, you may NOT use MimeDecode multiple times.
  Instead, you must use the MimeDecodePartial_ functions.
  See MimeDecodeStream for an example. }

function MimeDecodePartial (const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
function MimeDecodePartialEnd (out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;
{ The MimeDecodePartial_ functions are mostly for internal use.
  They serve the purpose of decoding very large data in multiple parts of
  smaller chunks, as used in MimeDecodeStream. }

implementation

const
 MIME_ENCODED_LINE_BREAK = 76;
 { According to RFC 1521, MIME_ENCODED_LINE_BREAK defaults to 76.
   If you ever need to change it, make sure it is a multiple of 4. }
 
 MIME_DECODED_LINE_BREAK = MIME_ENCODED_LINE_BREAK div 4 * 3;
 { Do not change this, even if you change MIME_ENCODED_LINE_BREAK above.
   MIME_DECODED_LINE_BREAK will always be a multiple of 3. }
 
 BUFFER_SIZE        = MIME_DECODED_LINE_BREAK * 3 * 4 * 16;
 { Fhe formula of BUFFER_SIZE is explained by the needs of MimeEncodeStream_
   and all other kinds of buffered Mime encodings (i.e. Files etc.).
   MimeEncodeFullLines only works if InputByteCount is a multiple of
   MIME_DECODED_LINE_BREAK. For MimeEncodeNoCRLF InputByteCount must be a
   multiple of 3 if used repeatedly, like in MimeEncodeStreamNoCRLF. In
   addition, a multiple of 4 makes sure memory is properly aligned. The factor
   16 just enlarges BUFFER_SIZE to a decent value. }
 
 MIME_ENCODE_TABLE  : array[0..63] of Byte = (
  065, 066, 067, 068, 069, 070, 071, 072, // 00 - 07
  073, 074, 075, 076, 077, 078, 079, 080, // 08 - 15
  081, 082, 083, 084, 085, 086, 087, 088, // 16 - 23
  089, 090, 097, 098, 099, 100, 101, 102, // 24 - 31
  103, 104, 105, 106, 107, 108, 109, 110, // 32 - 39
  111, 112, 113, 114, 115, 116, 117, 118, // 40 - 47
  119, 120, 121, 122, 048, 049, 050, 051, // 48 - 55
  052, 053, 054, 055, 056, 057, 043, 047); // 56 - 63
 
 MIME_PAD_CHAR      = Byte ('=');
 
 MIME_DECODE_TABLE  : array[Byte] of Cardinal = (
  255, 255, 255, 255, 255, 255, 255, 255, //  00 -  07
  255, 255, 255, 255, 255, 255, 255, 255, //  08 -  15
  255, 255, 255, 255, 255, 255, 255, 255, //  16 -  23
  255, 255, 255, 255, 255, 255, 255, 255, //  24 -  31
  255, 255, 255, 255, 255, 255, 255, 255, //  32 -  39
  255, 255, 255, 062, 255, 255, 255, 063, //  40 -  47
  052, 053, 054, 055, 056, 057, 058, 059, //  48 -  55
  060, 061, 255, 255, 255, 255, 255, 255, //  56 -  63
  255, 000, 001, 002, 003, 004, 005, 006, //  64 -  71
  007, 008, 009, 010, 011, 012, 013, 014, //  72 -  79
  015, 016, 017, 018, 019, 020, 021, 022, //  80 -  87
  023, 024, 025, 255, 255, 255, 255, 255, //  88 -  95
  255, 026, 027, 028, 029, 030, 031, 032, //  96 - 103
  033, 034, 035, 036, 037, 038, 039, 040, // 104 - 111
  041, 042, 043, 044, 045, 046, 047, 048, // 112 - 119
  049, 050, 051, 255, 255, 255, 255, 255, // 120 - 127
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255);
 
type
 PByte4 = ^TByte4;
 TByte4 = packed record
  b1: Byte;
  b2: Byte;
  b3: Byte;
  b4: Byte;
 end;
 
 PByte3 = ^TByte3;
 TByte3 = packed record
  b1: Byte;
  b2: Byte;
  b3: Byte;
 end;
 
 { ---------------------------------------------------------------------------- }
 { String Encoding & Decoding
 { ---------------------------------------------------------------------------- }
 
function MimeEncodeString (const s: AnsiString): AnsiString;
var
 l                  : Cardinal;
begin
 if Pointer (s) <> nil then
  begin
   l := Cardinal (Pointer (Cardinal (s) - 4)^);
   SetLength (Result, MimeEncodedSize (l));
   MimeEncode (Pointer (s)^, l, Pointer (Result)^);
  end
 else
  Result := '';
end;

{ ---------- }

function MimeEncodeStringNoCRLF (const s: AnsiString): AnsiString;
var
 l                  : Cardinal;
begin
 if Pointer (s) <> nil then
  begin
   l := Cardinal (Pointer (Cardinal (s) - 4)^);
   SetLength (Result, MimeEncodedSizeNoCRLF (l));
   MimeEncodeNoCRLF (Pointer (s)^, l, Pointer (Result)^);
  end
 else
  Result := '';
end;

{ ---------- }

function MimeDecodeString (const s: AnsiString): AnsiString;
var
 ByteBuffer, ByteBufferSpace: Cardinal;
 l                  : Cardinal;
begin
 if Pointer (s) <> nil then
  begin
   l := Cardinal (Pointer (Cardinal (s) - 4)^);
   SetLength (Result, (l + 3) div 4 * 3);
   ByteBuffer := 0;
   ByteBufferSpace := 4;
   l := MimeDecodePartial (Pointer (s)^, l, Pointer (Result)^, ByteBuffer, ByteBufferSpace);
   Inc (l, MimeDecodePartialEnd (Pointer (Cardinal (Result) + l)^, ByteBuffer, ByteBufferSpace));
   SetLength (Result, l);
  end
 else
  Result := '';
end;

{ ---------------------------------------------------------------------------- }
{ Stream Encoding & Decoding
{ ---------------------------------------------------------------------------- }

procedure MimeEncodeStream (const InputStream: TStream; const OutputStream: TStream);
var
 InputBuffer        : array[0..BUFFER_SIZE - 1] of Byte;
 OutputBuffer       : array[0.. (BUFFER_SIZE + 2) div 3 * 4 + BUFFER_SIZE div MIME_DECODED_LINE_BREAK * 2 - 1] of Byte;
 BytesRead          : Cardinal;
 IDelta, ODelta     : Cardinal;
begin
 BytesRead := InputStream.Read (InputBuffer, SizeOf (InputBuffer));
 
 while BytesRead = SizeOf (InputBuffer) do
  begin
   MimeEncodeFullLines (InputBuffer, SizeOf (InputBuffer), OutputBuffer);
   OutputStream.Write (OutputBuffer, SizeOf (OutputBuffer));
   BytesRead := InputStream.Read (InputBuffer, SizeOf (InputBuffer));
  end;

 MimeEncodeFullLines (InputBuffer, BytesRead, OutputBuffer);
 
 IDelta := BytesRead div MIME_DECODED_LINE_BREAK; // Number of lines processed.
 ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
 IDelta := IDelta * MIME_DECODED_LINE_BREAK;
 MimeEncodeNoCRLF (Pointer (Cardinal (@InputBuffer) + IDelta)^, BytesRead - IDelta, Pointer (Cardinal (@OutputBuffer) + ODelta)^);
 
 OutputStream.Write (OutputBuffer, MimeEncodedSize (BytesRead));
end;

{ ---------- }

procedure MimeEncodeStreamNoCRLF (const InputStream: TStream; const OutputStream: TStream);
var
 InputBuffer        : array[0..BUFFER_SIZE - 1] of Byte;
 OutputBuffer       : array[0.. ((BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
 BytesRead          : Cardinal;
begin
 BytesRead := InputStream.Read (InputBuffer, SizeOf (InputBuffer));
 while BytesRead = SizeOf (InputBuffer) do
  begin
   MimeEncodeNoCRLF (InputBuffer, SizeOf (InputBuffer), OutputBuffer);
   OutputStream.Write (OutputBuffer, SizeOf (OutputBuffer));
   BytesRead := InputStream.Read (InputBuffer, SizeOf (InputBuffer));
  end;
 
 MimeEncodeNoCRLF (InputBuffer, BytesRead, OutputBuffer);
 OutputStream.Write (OutputBuffer, (BytesRead + 2) div 3 * 4);
end;

procedure MimeDecodeStream (const InputStream: TStream; const OutputStream: TStream);
var
 ByteBuffer, ByteBufferSpace: Cardinal;
 InputBuffer        : array[0..BUFFER_SIZE - 1] of Byte;
 OutputBuffer       : array[0.. (BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
 BytesRead          : Cardinal;
begin
 ByteBuffer := 0;
 ByteBufferSpace := 4;
 BytesRead := InputStream.Read (InputBuffer, SizeOf (InputBuffer));
 while BytesRead > 0 do
  begin
   OutputStream.Write (OutputBuffer, MimeDecodePartial (InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
   BytesRead := InputStream.Read (InputBuffer, SizeOf (InputBuffer));
  end;
 OutputStream.Write (OutputBuffer, MimeDecodePartialEnd (OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

procedure DecodeHttpBasicAuthentication (const BasicCredentials: AnsiString; out UserId, PassWord: AnsiString);
label
 Fail;
const
 LBasic             = 6;                { Length ('Basic ') }
var
 DecodedPtr, p      : PAnsiChar;
 i, l               : Cardinal;
begin
 p := Pointer (BasicCredentials);
 if p = nil then goto Fail;
 
 l := Cardinal (Pointer (p - 4)^);
 if l <= LBasic then goto Fail;
 
 Dec (l, LBasic);
 Inc (p, LBasic);
 
 GetMem (DecodedPtr, (l + 3) div 4 * 3 { MimeDecodedSize (l) });
 l := MimeDecode (p^, l, DecodedPtr^);
 
 { Look for colon. }
 i := 0;
 p := DecodedPtr;
 while (l > 0) and (p[i] <> ':') do
  begin
   Inc (i);
   Dec (l);
  end;

 { Store UserId and Password. }
 SetString (UserId, DecodedPtr, i);
 if l > 1 then
  SetString (PassWord, DecodedPtr + i + 1, l - 1)
 else
  PassWord := '';
 
 FreeMem (DecodedPtr);
 Exit;
 
 Fail:
 UserId := '';
 PassWord := '';
end;

{ ---------------------------------------------------------------------------- }
{ Size Functions
{ ---------------------------------------------------------------------------- }

function MimeEncodedSize (const i: Cardinal): Cardinal;
begin
 Result := (i + 2) div 3 * 4 + (i - 1) div MIME_DECODED_LINE_BREAK * 2;
end;

function MimeEncodedSizeNoCRLF (const i: Cardinal): Cardinal;
begin
 Result := (i + 2) div 3 * 4;
end;

function MimeDecodedSize (const i: Cardinal): Cardinal;
begin
 Result := (i + 3) div 4 * 3;
end;

{ ---------------------------------------------------------------------------- }
{ Encoding Core
{ ---------------------------------------------------------------------------- }

procedure MimeEncode (const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
 IDelta, ODelta     : Cardinal;
begin
 MimeEncodeFullLines (InputBuffer, InputByteCount, OutputBuffer);
 IDelta := InputByteCount div MIME_DECODED_LINE_BREAK; // Number of lines processed so far.
 ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
 IDelta := IDelta * MIME_DECODED_LINE_BREAK;
 MimeEncodeNoCRLF (Pointer (Cardinal (@InputBuffer) + IDelta)^, InputByteCount - IDelta, Pointer (Cardinal (@OutputBuffer) + ODelta)^);
end;

procedure MimeEncodeFullLines (const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
 b, OuterLimit      : Cardinal;
 InPtr, InnerLimit  : ^Byte;
 OutPtr             : PByte4;
begin
 if InputByteCount = 0 then Exit;
 InPtr := @InputBuffer;
 OutPtr := @OutputBuffer;
 
 InnerLimit := InPtr;
 Inc (Cardinal (InnerLimit), MIME_DECODED_LINE_BREAK);
 
 OuterLimit := Cardinal (InPtr);
 Inc (OuterLimit, InputByteCount);
 
 { Multiple line loop. }
 while Cardinal (InnerLimit) <= OuterLimit do
  begin

   while InPtr <> InnerLimit do
    begin
     { Read 3 bytes from InputBuffer. }
     b := InPtr^;
     b := b shl 8;
     Inc (InPtr);
     b := b or InPtr^;
     b := b shl 8;
     Inc (InPtr);
     b := b or InPtr^;
     Inc (InPtr);
     { Write 4 bytes to OutputBuffer (in reverse order). }
     OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
     b := b shr 6;
     OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
     b := b shr 6;
     OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
     b := b shr 6;
     OutPtr^.b1 := MIME_ENCODE_TABLE[b];
     Inc (OutPtr);
    end;
   { Write line break (CRLF). }
   OutPtr^.b1 := 13;
   OutPtr^.b2 := 10;
   Inc (Cardinal (OutPtr), 2);

   Inc (InnerLimit, MIME_DECODED_LINE_BREAK);
  end;
 
end;

{ ---------- }

procedure MimeEncodeNoCRLF (const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
 b, OuterLimit      : Cardinal;
 InPtr, InnerLimit  : ^Byte;
 OutPtr             : PByte4;
begin
 if InputByteCount = 0 then Exit;
 InPtr := @InputBuffer;
 OutPtr := @OutputBuffer;
 
 OuterLimit := InputByteCount div 3 * 3;
 
 InnerLimit := @InputBuffer;
 Inc (Cardinal (InnerLimit), OuterLimit);

 { Last line loop. }
 while InPtr <> InnerLimit do
  begin
   { Read 3 bytes from InputBuffer. }
   b := InPtr^;
   b := b shl 8;
   Inc (InPtr);
   b := b or InPtr^;
   b := b shl 8;
   Inc (InPtr);
   b := b or InPtr^;
   Inc (InPtr);
   { Write 4 bytes to OutputBuffer (in reverse order). }
   OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
   b := b shr 6;
   OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
   b := b shr 6;
   OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
   b := b shr 6;
   OutPtr^.b1 := MIME_ENCODE_TABLE[b];
   Inc (OutPtr);
  end;
 
 { End of data & padding. }
 case InputByteCount - OuterLimit of
  1:
   begin
    b := InPtr^;
    b := b shl 4;
    OutPtr.b2 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr.b1 := MIME_ENCODE_TABLE[b];
    OutPtr.b3 := MIME_PAD_CHAR;         { Pad remaining 2 bytes. }
    OutPtr.b4 := MIME_PAD_CHAR;
   end;
  2:
   begin
    b := InPtr^;
    Inc (InPtr);
    b := b shl 8;
    b := b or InPtr^;
    b := b shl 2;
    OutPtr.b3 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr.b2 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr.b1 := MIME_ENCODE_TABLE[b];
    OutPtr.b4 := MIME_PAD_CHAR;         { Pad remaining byte. }
   end;
 end;
end;

{ ---------------------------------------------------------------------------- }
{ Decoding Core
{ ---------------------------------------------------------------------------- }

function MimeDecode (const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer): Cardinal;
var
 ByteBuffer, ByteBufferSpace: Cardinal;
begin
 ByteBuffer := 0;
 ByteBufferSpace := 4;
 Result := MimeDecodePartial (InputBuffer, InputBytesCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
 Inc (Result, MimeDecodePartialEnd (Pointer (Cardinal (@OutputBuffer) + Result)^, ByteBuffer, ByteBufferSpace));
end;

{ ---------- }

function MimeDecodePartial (const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
var
 lByteBuffer, lByteBufferSpace, c: Cardinal;
 InPtr, OuterLimit  : ^Byte;
 OutPtr             : PByte3;
begin
 if InputBytesCount > 0 then
  begin
   InPtr := @InputBuffer;
   Cardinal (OuterLimit) := Cardinal (InPtr) + InputBytesCount;
   OutPtr := @OutputBuffer;
   lByteBuffer := ByteBuffer;
   lByteBufferSpace := ByteBufferSpace;
   while InPtr <> OuterLimit do
    begin
     { Read from InputBuffer. }
     c := MIME_DECODE_TABLE[InPtr^];
     Inc (InPtr);
     if c = $FF then Continue;
     lByteBuffer := lByteBuffer shl 6;
     lByteBuffer := lByteBuffer or c;
     Dec (lByteBufferSpace);
     { Have we read 4 bytes from InputBuffer? }
     if lByteBufferSpace <> 0 then Continue;

     { Write 3 bytes to OutputBuffer (in reverse order). }
     OutPtr^.b3 := Byte (lByteBuffer);
     lByteBuffer := lByteBuffer shr 8;
     OutPtr^.b2 := Byte (lByteBuffer);
     lByteBuffer := lByteBuffer shr 8;
     OutPtr^.b1 := Byte (lByteBuffer);
     lByteBuffer := 0;
     Inc (OutPtr);
     lByteBufferSpace := 4;
    end;
   ByteBuffer := lByteBuffer;
   ByteBufferSpace := lByteBufferSpace;
   Result := Cardinal (OutPtr) - Cardinal (@OutputBuffer);
  end
 else
  Result := 0;
end;

{ ---------- }

function MimeDecodePartialEnd (out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;
var
 lByteBuffer        : Cardinal;
begin
 case ByteBufferSpace of
  1:
   begin
    lByteBuffer := ByteBuffer shr 2;
    PByte3 (@OutputBuffer)^.b2 := Byte (lByteBuffer);
    lByteBuffer := lByteBuffer shr 8;
    PByte3 (@OutputBuffer)^.b1 := Byte (lByteBuffer);
    Result := 2;
   end;
  2:
   begin
    lByteBuffer := ByteBuffer shr 4;
    PByte3 (@OutputBuffer)^.b1 := Byte (lByteBuffer);
    Result := 1;
   end;
  else
   Result := 0;
 end;
end;

end.

