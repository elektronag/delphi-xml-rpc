
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/source/xmlrpcsynchclasses.pas,v 1.1.1.1 2003-11-19 22:12:12 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit xmlrpcsynchclasses;

{$IFDEF VER140}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

interface

uses
  {$IFDEF WIN32}
  Windows,
  messages,     { TMessage }
  {$ENDIF}
  classes,
  SyncObjs     { TCriticalSection }
  ;

  { $DEFINE NO_LOCK}
  { debug - turn off locking }

  { synchroniser to allow callbacks to specific thread contexts }
  { windows only code in here - sorry }

  {$IFDEF WIN32}
type TWinSynchroniser = class
  private
    fCallBackContext : Cardinal;
    fWindowHandle : HWND;
    fMethod : TNotifyEvent;
    fLock : TCriticalSection;
    procedure SetMethod(const Value: TNotifyEvent);
    procedure SetContext;
    procedure DoMethod(Sender : TObject);
    procedure WndProc(var Msg: TMessage);
    procedure GetHandle;
    procedure FreeHandle;
    procedure Lock;
    procedure Unlock;
  public
    procedure RunMethod(Sender : TObject ; ASynch : boolean = false);
    property Method : TNotifyEvent read fMethod write SetMethod;
    constructor Create;
    destructor Destroy; override;

end;

{$ENDIF}

{$IFDEF LINUX}
type
  TDummyThread = class(TThread)
  public
    procedure Execute; override;
    procedure SynchMethod(Method : TThreadMethod);
  end;



type
  TCLXSynchroniser = class
  private
    fDummyThread : TDummyThread;
    fSender : TObject;
    fMethod: TNotifyEvent;
    procedure DoMethod;
  public
    procedure RunMethod(Sender : TObject ; ASynch : boolean = false);
    property Method : TNotifyEvent read fMethod write fMethod;
    constructor Create;
    destructor Destroy; override;
  end;

{$ENDIF}

{ alias the desired type }


type
  {$IFDEF WIN32}
  TSynchroniser = TWinSynchroniser;
  {$ENDIF}
  {$IFDEF LINUX}
  TSynchroniser = TCLXSynchroniser;
  {$ENDIF}


implementation

uses
  {$IFDEF WIN32}
  Forms,        { AllocHwnd }
  {$ENDIF}
  SysUtils      { Exception }
  ;

  {$IFDEF WIN32}

const
  RM_SYNCH = WM_APP;


{ TWinSynchroniser }

constructor TWinSynchroniser.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  { initially set to creator context }
  SetContext;
end;

destructor TWinSynchroniser.Destroy;
begin
  { free window handle }
  FreeHandle;
  FLock.Free;
  inherited;

end;

procedure TWinSynchroniser.DoMethod(Sender: TObject);
begin
  if Assigned(fMethod) then
    fMethod(Sender);
end;

procedure TWinSynchroniser.FreeHandle;
begin
  Lock;
  try
    if (fWindowHandle <> 0) then
    begin
      DeallocateHWnd(fWindowHandle);
      fWindowHandle := 0;
    end;
  finally
    Unlock;
  end;
end;

procedure TWinSynchroniser.GetHandle;
begin
  Lock;
  try
    if (fWindowHandle = 0) then
      fWindowHandle := AllocateHwnd(WndProc);
  finally
    Unlock;
  end;
end;

procedure TWinSynchroniser.Lock;
begin
  FLock.Acquire;
end;

procedure TWinSynchroniser.RunMethod(Sender: TObject ; ASynch : boolean = false);
begin
  if (GetCurrentThreadId <> fCallBackContext) then
  begin
    SendMessage(fWindowHandle, RM_SYNCH, Integer(Sender), Integer(ASynch));
  end
  else
  begin
    DoMethod(Sender);
  end;
end;

procedure TWinSynchroniser.SetContext;
begin
  { this is also protected to prevent odd behaviour when the handle is undefined }
  Lock;
  try
    { tests for whether context has been altered }
    if (fCallBackContext = 0) then
    begin
      fCallBackContext := GetCurrentThreadID;
      GetHandle;
    end
    else
    if (fCallBackContext <> GetCurrentThreadID) then
    begin
      fCallBackContext := GetCurrentThreadID;
      FreeHandle;
      GetHandle;
    end;
  finally
    UnLock;
  end;

end;

procedure TWinSynchroniser.SetMethod(const Value: TNotifyEvent);
begin
  Lock;
  try
    fMethod := Value;
  finally
    UnLock;
  end;

end;

procedure TWinSynchroniser.Unlock;
begin
  FLock.Release;
end;

procedure TWinSynchroniser.WndProc(var Msg: TMessage);
begin
  try
    if Msg.Msg = RM_SYNCH then
      begin
        { if want caller to continue to run asynchronously }
        { this is OK to do, if we're just notifying someonse,
          but passing no modifiable data }
        if (boolean(Msg.LParam)) then
          ReplyMessage(0);
        DoMethod(TObject(Msg.WParam));
      end
  except
    {}
  end

end;

{$ENDIF}


{$IFDEF LINUX}

{ TCLXSynchroniser }

constructor TCLXSynchroniser.Create;
begin
  fDummyThread := TDummyThread.Create(true);
end;

destructor TCLXSynchroniser.Destroy;
begin
  fDummyThread.Free;
  inherited;

end;

procedure TCLXSynchroniser.DoMethod;
begin
  fMethod(fSender);
end;

procedure TCLXSynchroniser.RunMethod(Sender: TObject; ASynch: boolean);
begin
  fSender := Sender;
  fDummyThread.SynchMethod(DoMethod);
end;


{ TDummyThread }

procedure TDummyThread.Execute;
begin
  { nothing }
end;

procedure TDummyThread.SynchMethod(Method: TThreadMethod);
begin
  Synchronize(Method);
end;

{$ENDIF}

end.
