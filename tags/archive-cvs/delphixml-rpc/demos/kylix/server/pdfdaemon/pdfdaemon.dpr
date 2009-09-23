
{*******************************************************}
{                                                       }
{ XML-RPC Library for Delphi, Kylix and DWPL (DXmlRpc)  }
{                                                       }
{ for Delphi 5 - 7                                      }
{ Release 1.x.x                                         }
{ Copyright (c) 2001-2003 by Team-DelphiXml-Rpc         }
{ e-mail: team-dxmlrpc@dwp42.org                        }
{ www: http://sourceforge.net/projects/delphixml-rpc/   }
{                                                       }
{ The initial developer of the code is                  }
{   Clifford E. Baeseman, codepunk@codepunk.com         }
{                                                       }
{ This file may be distributed and/or modified under    }
{ the terms of the GNU Lesser General Public License    }
{ (LGPL) version 2.1 as published by the Free Software  }
{ Foundation and appearing in the included file         }
{ license.txt.                                          }
{                                                       }
{*******************************************************}
{
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/demos/kylix/server/pdfdaemon/pdfdaemon.dpr,v 1.1.1.1 2003-11-19 22:11:25 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
program pdfdaemon;

{$APPTYPE CONSOLE}

uses
  Libc,
  SysUtils,
  rpcserver in 'rpcserver.pas';

Var
   { vars for daemonizing }
   bHup,
   bTerm : boolean;
   fLog : Text;
   logname : string;
   aOld,
   aTerm,
   aHup : PSigAction;   //was pSigActionRec
   ps1  : psigset;
   sSet : cardinal;
   pid : longint;
   secs : longint;
   FServer: TRPCServer;


{ handle SIGHUP & SIGTERM }
procedure DoSig(sig : longint);cdecl;
begin
   case sig of
      SIGHUP : bHup := true;
      SIGTERM : bTerm := true;
   end;
end;

{ open the log file }
Procedure NewLog;
Begin
   Assignfile(fLog,logname);
   Rewrite(fLog);
   Writeln(flog,'Indy TCP DaemonLog created at '+ formatdatetime('dd/mm/yyyy',now));
   Closefile(fLog);
End;

begin
    logname := '/var/log/pdfdaemon.log';
    secs := 10;

    { set global daemon booleans }
    bHup := true; { to open log file }
    bTerm := false;

    { block all signals except -HUP & -TERM }
    sSet := $ffffbffe;
    ps1 := @sSet;
    sigprocmask(sig_block,ps1,nil);

   { setup the signal handlers }
   new(aOld);
   new(aHup);
   new(aTerm);
   aTerm^.__sigaction_handler := @DoSig;
   aTerm^.sa_flags := 0;
   aTerm^.sa_restorer := nil;
   aHup^.__sigaction_handler:= @DoSig;
   aHup^.sa_flags := 0;
   aHup^.sa_restorer := nil;
   SigAction(SIGTERM,aTerm,aOld);
   SigAction(SIGHUP,aHup,aOld);

   { daemonize }
   pid := Fork;
   Case pid of
      0 : Begin { we are in the child }
             Close(input);  { close standard in }
             AssignFile(output,'/dev/null');
             ReWrite(output);
             AssignFile(ErrOutPut,'/dev/null');
             ReWrite(ErrOutPut);
          End;
      -1 : secs := 0;     { forking error, so run as non-daemon }
      Else Halt;          { successful fork, so parent dies }

   End;  //case
   { Create any objects before you go into the processing loop}
    FServer := TRPCServer.Create;
   { begin processing loop }
   Repeat
      If bHup Then Begin
         NewLog;
         bHup := false;
      End;
      {----------------------}
      { Do your daemon stuff }
      

       Append(flog);
       Writeln(flog,'pdfdaemon started at '+pchar(formatdatetime('mm/dd/yyy hh:mm:ss',now)));
        Close(fLog);
      { the following output goes to the bit bucket }
        Writeln('pdfdaemon activated at '+pchar(formatdatetime('mm/dd/yyy hh:mm:ss',now)));

      {----------------------}
      If (bTerm) Then
         begin
          FServer.Free;
          BREAK
         end

             Else
                 { wait a while }
                 __sleep(secs*1000);


   Until bTerm;

end.
