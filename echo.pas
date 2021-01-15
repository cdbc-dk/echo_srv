unit echo;
{$mode objfpc}{$H+}
interface

uses
  Classes, LMessages, LCLIntf,
  bc_mtlinklist,
  blcksock,
  synsock;

const
  UnitVersion = '3.07.04.2020';
  { internal messages for use between daemon & app }
  LM_CREATE  = LM_USER+1;
  LM_LISTEN  = LM_USER+3;
  LM_ACCEPT  = LM_USER+5;
  LM_WORKING = LM_USER+7;
  LM_DONE    = LM_USER+11;
  LM_DESTROY = LM_USER+13;
  LM_LOADED  = LM_USER+17;
  LM_SENDING = LM_USER+19; { 07.04.2020 /bc }
  LM_SENT    = LM_USER+23; { 07.04.2020 /bc }
  LM_ERROR   = LM_USER+31; { 07.04.2020 /bc }
  { result codes from functions, more flexible than boolean }
  HR_OK = 0;
  HR_ERROR = -1;

type
  TTCPEchoThrd = class; { forward declaration }
  { TbcMessage }
  TbcMessage = class
  private
    fLParam: ptrint;
    fMsg: ptrint;
    fSParam: string;
    fWParam: ptrint;
    function CloneString(const aString: string): string;
  public
    constructor Create(aMsg: ptrint;aWParam: ptrint;aLParam: ptrint;aSParam: string);
    property Msg: ptrint read fMsg write fMsg;
    property WParam: ptrint read fWParam write fWParam;
    property LParam: ptrint read fLParam write fLParam;
    property SParam: string read fSParam write fSParam; // optional string param
  end;

  { a message queue for thread uncupling/detachment }
  { TbcMessageQueue }
  TbcMessageQueue = class(TaaQueue)
    procedure EnQueue(aMessage: TbcMessage);
    function Peek: TbcMessage; { does NOT remove from queue, only looking }
    function DeQueue: TbcMessage;
  end;
  { TCmdEvent }
  TCmdEvent = procedure(aPeerThread: TTCPEchoThrd;const aCmd: string) of object;

  { server thread }
  TTCPEchoDaemon = class(TThread)
  private
    Sock:TTCPBlockSocket;
    fHandle: THandle;
    fOnCmdEvent: TCmdEvent;
  public
    Constructor Create(const aHandle: THandle;aCmdHandler: TCmdEvent);
    Destructor Destroy; override;
    property OnCmdReceived: TCmdEvent read fOnCmdEvent write fOnCmdEvent;
    procedure Execute; override;
  end;

  { TTCPEchoThrd }
  TTCPEchoThrd = class(TThread)
  private
    fSock:TTCPBlockSocket;
    CSock: TSocket;
    fHandle: THandle;
    fOnCmdEvent: TCmdEvent;
  protected
    procedure DoOnCmdReceived(const aCmd: string);
  public
    Constructor Create(hsock: tSocket;const aHandle: THandle;aCmdHandler: TCmdEvent);
    property Sock: TTCPBlockSocket read fSock write fSock;
    property OnCmdReceived: TCmdEvent read fOnCmdEvent write fOnCmdEvent;
    procedure Execute; override;
  end;

  { utility functions }
  function StrCase(const S: string;aChoice: array of string): ptrint;
  function StrCase5(const S: string; aChoice: array of string): ptrint;
  function MsgToStr(const aMessage: ptruint): string;

var { global messagequeue }
  MsgQueue: TbcMessageQueue;

implementation
{ zero-based strcase function, ie. first choice = 0 }
function StrCase(const S: string; aChoice: array of string): ptrint;
var Idx: integer;
begin
  Result:= -1;
  for Idx:= low(aChoice) to high(aChoice) do if S = aChoice[Idx] then begin
    Result:= Idx;
    break;
  end;
end;

function StrCase5(const S: string; aChoice: array of string): ptrint;
var Idx: integer;
begin
  Result:= -1;
  for Idx:= low(aChoice) to high(aChoice) do if copy(S,1,5) = aChoice[Idx] then begin
    Result:= Idx;
    break;
  end;
end;

function MsgToStr(const aMessage: ptruint): string;
begin
  case aMessage of
    LM_CREATE  : Result:= 'Create';
    LM_LISTEN  : Result:= 'Listen';
    LM_ACCEPT  : Result:= 'Accept';
    LM_WORKING : Result:= 'Working';
    LM_DONE    : Result:= 'Done!';
    LM_DESTROY : Result:= 'Destroy';
    LM_LOADED  : Result:= 'Loaded';
    LM_SENDING : Result:= 'Sending'; { 07.04.2020 /bc }
    LM_SENT    : Result:= 'Sent'; { 07.04.2020 /bc }
    LM_ERROR   : Result:= 'Error!'; { 07.04.2020 /bc }
  else
    Result:= 'Unknown';
  end;
end;

{ TbcMessageQueue }
procedure TbcMessageQueue.EnQueue(aMessage: TbcMessage);
begin
  En_Queue(pointer(aMessage));
  Notify(aMessage,qnEnqueued);
end;

function TbcMessageQueue.Peek: TbcMessage;
begin
  Result:= TbcMessage(Examine);
  Notify(Result,qnExamined);
end;

function TbcMessageQueue.DeQueue: TbcMessage;
begin
  Result:= TbcMessage(De_Queue);
  Notify(Result,qnDequeued);
end;

{ TbcMessage }
function TbcMessage.CloneString(const aString: string): string;
var Len: ptrint;
begin
  Len:= Length(aString);
  SetLength(Result,Len);
  move(aString[1],Result[1],Len); // should make it unique?!?
end;

constructor TbcMessage.Create(aMsg: ptrint; aWParam: ptrint; aLParam: ptrint; aSParam: string);
begin
  inherited Create;
  fMsg:= aMsg;
  fWParam:= aWParam;
  fLParam:= aLParam;
  fSParam:= CloneString(aSParam); { should be unique?!? }
end;

{ TEchoDaemon }
Constructor TTCPEchoDaemon.Create(const aHandle: THandle;aCmdHandler: TCmdEvent);
begin
  inherited create(true);
  fHandle:= aHandle;
  fOnCmdEvent:= aCmdHandler;
  sock:=TTCPBlockSocket.create;
  FreeOnTerminate:=true;
  MsgQueue.EnQueue(TbcMessage.Create(LM_CREATE,8723,ThreadID,'TTcpEchoDaemon created...'));
//  PostMessage(fHandle,LM_CREATE,8723,longint(pchar('Echo daemon created.')));
  Start;
end;

Destructor TTCPEchoDaemon.Destroy;
begin
  Sock.free;
  MsgQueue.EnQueue(TbcMessage.Create(LM_DESTROY,8723,ThreadID,'TTcpEchoDaemon destroyed.'));
//  PostMessage(fHandle,LM_DESTROY,0,longint(pchar('Echo daemon destroyed.')));
end;

procedure TTCPEchoDaemon.Execute;
var
  ClientSock:TSocket;
begin
  with Sock do begin
    CreateSocket;
    SetLinger(true,10000); { linger around / wait for awhile }
    Bind('0.0.0.0','8723'); { bind to every interface on port 8723 }
    Listen; { wait for someone to knock on the door :-) }
    MsgQueue.EnQueue(TbcMessage.Create(LM_LISTEN,8723,ThreadID,'Echo daemon is listening...'));
//    PostMessage(fHandle,LM_LISTEN,0,longint(pchar('Echo daemon is listening...')));
    repeat
      if Terminated then break;
      if CanRead(1000) then begin
        ClientSock:= Accept; { accept the new connection and create a socket handle }
        MsgQueue.EnQueue(TbcMessage.Create(LM_ACCEPT,8723,ThreadID,'Echo daemon accepted client.'));
        { now hand the socket off to a new thread and do the actual work }
        if LastError = 0 then TTCPEchoThrd.Create(ClientSock,fHandle,fOnCmdEvent);
//        PostMessage(fHandle,LM_ACCEPT,8723,longint(pchar('Echo daemon accepted client...')));
      end;
    until false;
  end;
  MsgQueue.EnQueue(TbcMessage.Create(LM_DONE,8723,ThreadID,'Echo daemon is done!'));
end;

{ TEchoThrd }
procedure TTCPEchoThrd.DoOnCmdReceived(const aCmd: string);
begin
  if Assigned(fOnCmdEvent) then fOnCmdEvent(Self,aCmd);
end;

constructor TTCPEchoThrd.Create(hSock: TSocket;const aHandle: THandle;aCmdHandler: TCmdEvent);
begin
  inherited Create(true); { create suspended to allow properties to be set }
  fHandle:= aHandle; { a handle to the main form to receive messages }
  fOnCmdEvent:= aCmdHandler; { connect forms event handler }
  CSock:= hSock; { worker socket }
  FreeOnTerminate:=true; { when done, go away }
  Start; { run forrest run... }
end;

procedure TTCPEchoThrd.Execute;
var S: string;
begin
  fSock:=TTCPBlockSocket.create; { setup the worker socket }
  MsgQueue.EnQueue(TbcMessage.Create(LM_CREATE,8723,ThreadID,'TTcpEcho thread created...'));
  try
    fSock.Socket:= CSock; { connect to the daemon socket }
    fSock.GetSins; { setup local and remote parameters }
    with fSock do begin
      repeat
        if Terminated then break;
        S:= RecvPacket(60000); { receive data }
        if LastError <> 0 then break; { check for errors }
        MsgQueue.EnQueue(TbcMessage.Create(LM_WORKING,8723,ThreadID,'Received: '+S));
        if copy(S,6,1) = '|' then begin
          MsgQueue.EnQueue(TbcMessage.Create(LM_WORKING,8723,ThreadID,'Processing command.'));
          DoOnCmdReceived(S); { trigger event delegates }
        end else SendString(S); { echo data back :-) }
        if LastError <> 0 then break;
 //       PostMessage(fHandle,LM_WORKING,0,longint(pchar('Worker thread is running.')));
      until false;
    end;
  finally fSock.Free; end;
  MsgQueue.EnQueue(TbcMessage.Create(LM_DONE,8723,ThreadID,'TTcpEcho thread is done!'));
//  PostMessage(fHandle,LM_DONE,0,longint(pchar('Worker thread is done!.')));
end;

initialization
  MsgQueue:= TbcMessageQueue.Create; { setup a global messagequeue }
finalization
  MsgQueue.Free; { tear down the global messagequeue }
end.
