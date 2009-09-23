Delphi / Kylix XML-RPC Client

Copyright Clifford E. Baeseman <codepunk@codepunk.com>
Please supply all bug fix requests to codepunk@codepunk.com you
will find me very responsive to problems.

Licensed under the LGPL See the included license file. 

Developer Credits
 Major Source Contributions and Patches
 by Patrick Martin
 
 Server Thread Syncronization
 Memory Hole Cleanup
  

 This library contains a mime encoder by
 Ralf Junker <ralfjunker@gmx.de> 2000-2001
 http://www.zeitungsjunge.de/delphi/ 

 This library also contains the excellent xml parser
 written by Stefan Heymann.
 http://www.destructor.de


YOU MUST INSTALL THE LATEST INDY COMPONENTS TO COMPILE!
http://www.nevrona.com/Indy/


Change Log:
   Release 1.7.7
   April 07, 2002
   Merged a struct fix from Avi Lewin
   Bug Fix affecting integer resolution


   Release 1.7.5
   Dec 05, 2001
   Patric Martin
   Merged Major Contributions From Patrick 
   Memory Hole Cleanups
   Server Thread Syncronization
   Server Thread Syncronization Server examples

   Codepunk
   Kylix Server Daemon Example (Text to PDF Converter)
     

   Dec 03, 2001
   Release 1.7.4
   Response tag fix in server unit
   Added Introspect capability to server
   Added Introspect Client
   Fixed a few memory holes
   Fixed Kylix 2.0 compile problem, in stock kylix2 with indy 8
   just compile as usual. For indy9 and above compile with INDY9 conditional
   to get full functionality.
   Tested in Delphi 5, 6 as well as Kylix Pro 1 and Kylix OE 2.0

   Nov 26, 2001
   Release 1.7.3
   Completed initial server code and conducted some testing
   Added a very simple client and server code example

   Nov 26, 2001
   Release 1.7.2
   Bug fix in addrawdata for date type
   Normalize whitespace bug fix
   Much code cleanup
   Initial server code added, not ready for prime time 
   but very close.

   Nov  13, 2001
   Release 1.7.1
   Two bug fixes 
   (I4 issue and strings honoring whitespace)
   
   Nov 11, 2001
   Release 1.7
   Old xml parser replaced with industrial
   strength model.
   Reworked the entire result object stack. The result
   is much cleaner and faster.
   Placed in new stubs for SSL enable. This is not fully tested as
   of yet.

   Oct 17,2001
   Release 1.6.1
   Had to back out new parsing engine due
   to flaws in dealing with structs and arrays..
   Implemented very nasty empty string hack but
   should cause no problems.

   Oct 15, 2001
   Release 1.6
   (merged patches)
   International Date Fix (S. R. Oddson)
   (other changes)
   Removed MD5 Unit (C. Baeseman)
   Tested Kylix OE compatibility
   Fixed bug in error handler

   Sept 18, 2001
   Release 1.5
   Applied parsing bug fix submitted by 
   S. R. Oddsson (Thanks for the input!)
   Applied port bug fix for ports other than default 80
   Applied compatibility fix for out of order error faults
   produced by the helma java implementaion

   April 22, 2001
    Release 1.1
    Applied one bug fixed related to 
    deeply nested structs

   April 18, 2001
    Total Interface Overhaul
    Client Now Fully validates 
    Added RC4 Encryption Methods
    Now Handles Nested Structs and Arrays Properly

   April 7, 2001
   Version 0.8
     Added IsConnected
     Added Progress Events 
     Fixed Bug while talking to MS Servers(100 Continue)
     Added Proxy Settings
     Requires Indy components to build.
         
   April 2, 2001
   Version 0.7
     Merged Win32 and Kylix Branches
     Retested Validation
     Changed Internet Component to indy  http://www.nevrona.com/indy/
     You must have 8.0 installed to compile. And the beta source files.

   April 1, 2001
   Initial Release 
     I tested a small amount after the conversion and found 
     no real issues.     

