
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/source/xmlrpcthreadallocator.pas,v 1.1.1.1 2003-11-19 22:12:13 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit xmlrpcthreadallocator;
{$R-}
interface

uses
  syncobjs,     { TCriticalSection }
  classes       { TThreadList }
  ;

type
  TObjectCreationProc = function : TObject of object;

type

  TThreadObjectHolder = class
    private
      ObjectInstance : TObject;
    //  ObjectThreadID : Cardinal;
      ObjectThreadID: Integer;
    public
      constructor Create;
      destructor Destroy; override;
  end;

type
  TThreadAllocator = class
    private
      fInstanceList : TThreadList;
      FCreationProc: TObjectCreationProc;
      function GetThreadObject: TObject;

    public
      constructor Create;
      destructor Destroy; override;
      property CreationProc : TObjectCreationProc read FCreationProc write fCreationProc;
      property ThreadObject : TObject read GetThreadObject;

  end;

implementation
  uses
    {$IFDEF WIN32}
    Windows,    { GetCurrentThreadID }
    {$ENDIF}
    sysutils    { Exception }
    ;

{ ThreadAllocator }

  { bizarrely, this is declared in QForms, but not exposed }

function GetCurrentThreadID: Integer;
{$IFDEF WIN32}
  external 'kernel32.dll' name 'GetCurrentThreadId';
{$ENDIF}
{$IFDEF LINUX}
  external 'libpthread.so.0' name 'pthread_self';
{$ENDIF}



constructor TThreadAllocator.Create;
begin
  fInstanceList := TThreadList.Create;
end;

destructor TThreadAllocator.Destroy;
begin
  with fInstanceList.LockList do
  try
    while Count > 0 do
    begin
      TObject(Items[0]).Free;
      Delete(0);
    end;
  finally
    fInstanceList.UnlockList;
    fInstanceList.Free;
  end;
  inherited;

end;

function TThreadAllocator.GetThreadObject: TObject;
var
  i : integer;
  ThreadObjectHolder : TThreadObjectHolder;
begin
  if not Assigned(FCreationProc) then
    raise Exception.Create('don''t know what to make!');
  Result := nil;

  with fInstanceList.LockList do
  try
    {not I'm not using exit because
     [a] - it's poor coding style
     [b] - exit is an abnormal termination,
           and all the try/finally handlers get run in an unwind
           - performance hit!}
    for i := 0 to Count - 1 do
    begin
      if (GetCurrentThreadId =
        TThreadObjectHolder(Items[i]).ObjectThreadID) then
        begin
          Result := TThreadObjectHolder(Items[i]).ObjectInstance;
        end;
    end;

    if Result = nil then
    begin
      { create and add }
      Result := FCreationProc;
      ThreadObjectHolder := TThreadObjectHolder.Create;
      ThreadObjectHolder.ObjectInstance := Result;
      ThreadObjectHolder.ObjectThreadID := GetCurrentThreadId;
      Add(ThreadObjectHolder);
    end;

  finally
    fInstanceList.UnlockList;
  end;

end;


{ TThreadObjectHolder }


constructor TThreadObjectHolder.Create;
begin
  {}
end;

destructor TThreadObjectHolder.Destroy;
begin
  { this is owning the objects }
  if Assigned(ObjectInstance) then
    ObjectInstance.Free;
  inherited;

end;


end.
