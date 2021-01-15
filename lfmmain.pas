unit lfmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, LCLIntf, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  echo,
  bc_mtlinklist,
  bc_strings,
  bc_datetime;

type
  { messagequeue observer }
  TMessageQueueObserver = class(TInterfacedObject,IFPObserver)
  private
    fMessage: TbcMessage;
  public
    constructor Create;
    destructor Destroy; override;
    Procedure FPOObservedChanged(aSender: TObject;anOperation: TFPObservedOperation;aData: pointer);
    property Message: TbcMessage read fMessage;
  end;

  { TfrmMain }
  TfrmMain = class(TForm)
    btnStartSrv: TButton;
    btnStopSrv: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure btnStartSrvClick(Sender: TObject);
    procedure btnStopSrvClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fObserver: TMessageQueueObserver;
    fEchoSrv: TTCPEchoDaemon;
    fMainThreadId: ptruint;
    fStream: TMemoryStream;
    function SendList(aPeerThread: TTCPEchoThrd): ptrint; { send a list of commands to client }
    function SendFile(aPeerThread: TTCPEchoThrd;const aFilename: string): ptrint;
  protected { message based comm, using "Postmessage" }
    procedure LMCreate(var Message: TLMessage); message LM_CREATE;
    procedure LMListen(var Message: TLMessage); message LM_LISTEN;
    procedure LMAccept(var Message: TLMessage); message LM_ACCEPT;
    procedure LMWorking(var Message: TLMessage); message LM_WORKING;
    procedure LMDone(var Message: TLMessage); message LM_DONE;
    procedure LMDestroy(var Message: TLMessage); message LM_DESTROY;
  public { thread & queue events }
    procedure ThreadTerminated(Sender: TObject);
    procedure DataInQueue(Sender: TObject);
    procedure CmdHandler(aPeerThread: TTCPEchoThrd;const aCmd: string);
  end;

var
  frmMain: TfrmMain;

implementation
{$R *.lfm}
uses bc_errorlog;
procedure ShowToday;
var Dt: IIsoDateTime;
begin
  Dt:= TIsoDateTime.Create;
  ShowMessageFmt('Date- AsInteger: %d, AsString: %s, AsDate: %f, Time- AsInteger: %d, AsString: %s, AsTime: %f',
                 [Dt.Date.AsInteger,Dt.Date.AsString,Dt.Date.AsDate,Dt.Time.AsInteger,Dt.Time.AsString,Dt.Time.AsTime]);
  Dt:= nil;
end;

{ *** TMessageQueueObserver *** }
constructor TMessageQueueObserver.Create;
begin
  fMessage:= nil;
end;

destructor TMessageQueueObserver.Destroy;
begin
  if assigned(fMessage) then fMessage.Free;
  inherited Destroy;
end;

procedure TMessageQueueObserver.FPOObservedChanged(aSender: TObject;
                                                   anOperation: TFPObservedOperation;
                                                   aData: pointer);
const fmtMsg = 'msg: %s, wparam: %d, lparam: %d, sparam: %s';
begin
  fMessage:= TbcMessage(aData);
  case anOperation of
    ooAddItem   : ErrorLog.LogLn(format(fmtMsg+' enqueued',[MsgToStr(fMessage.Msg),
                                                            fMessage.WParam,
                                                            fMessage.LParam,
                                                            fMessage.SParam]));
    ooCustom    : ErrorLog.LogLn(format(fmtMsg+' examined',[MsgToStr(fMessage.Msg),
                                                            fMessage.WParam,
                                                            fMessage.LParam,
                                                            fMessage.SParam]));
    ooDeleteItem: ErrorLog.LogLn(format(fmtMsg+' dequeued',[MsgToStr(fMessage.Msg),
                                                            fMessage.WParam,
                                                            fMessage.LParam,
                                                            fMessage.SParam]));
  end;
end;
{ *** TMessageQueueObserver *** }

{ *** TfrmMain *** }
procedure TfrmMain.btnStartSrvClick(Sender: TObject);
begin
  btnStartSrv.Enabled:= false;
  fEchoSrv:= echo.TTCPEchoDaemon.Create(Handle,@CmdHandler);
  btnStopSrv.Enabled:= true;
end;

procedure TfrmMain.btnStopSrvClick(Sender: TObject);
begin
  btnStopSrv.Enabled:= false;
  fEchoSrv.Terminate;
  fEchoSrv.WaitFor;
  btnStartSrv.Enabled:= true;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  btnStopSrv.Enabled:= false;
  fObserver:= TMessageQueueObserver.Create;
  MsgQueue.FPOAttachObserver(fObserver);
  Caption:= 'Echo Server - [Port: 8723]';
  fMainThreadId:= ThreadID;
  Memo1.Lines.Text:= 'Main Thread Id: '+inttostr(fMainThreadId);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  MsgQueue.FPODetachObserver(fObserver);
  fObserver.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin { connect the messagequeue event with this form }
  echo.MsgQueue.OnDataInQueue:= @DataInQueue; { beware runs in a separat thread }
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
const FmtString = ', wparam: %d, lparam: %d, sparam: %s';
var Msg: TbcMessage;
begin
  if not echo.MsgQueue.IsEmpty then begin
    Msg:= echo.MsgQueue.DeQueue; { get the first message in queue, if not empty }
    Memo1.Lines.Add(format('msg: '+MsgToStr(Msg.Msg)+FmtString,[Msg.WParam,Msg.LParam,Msg.SParam]));
    FreeAndNil(Msg); { users of the message queue, are responsible for freeing the message }
  end;
end;

function TfrmMain.SendList(aPeerThread: TTCPEchoThrd): ptrint;
begin { send a list of commands to client }
  Result:= HR_ERROR;
  with TStringList.Create do begin
    Add('List of commands i understand: ');
    Add(' 1) .list = this list of commands.');
    Add(' 2) .recv = receive a file. ex.: .recv|/home/bc/src/filethreads.pas');
    Add(' 3) .send = send a file. ex.: .send|/home/bc/src/filethreads.pas');
    Add(' 4) .info = receive some info. ex.: .info|/home/bc/src/*.pas');
    Add(' 5) .echo = echoes your input string. ex.: .echo|The brown fox jumped over the lazy dog.');
    Add(' 6) .quit = bye bye. ex.: .quit|');
    Add('End of list.');
    MsgQueue.EnQueue(TbcMessage.Create(LM_SENDING,Count,GetTickCount,'Sending List.'));
    if aPeerThread.Sock.CanWrite(2000) then aPeerThread.Sock.SendString(Text);
    if aPeerThread.Sock.LastError = 0 then begin
      Result:= HR_OK;
      MsgQueue.EnQueue(TbcMessage.Create(LM_SENT,Count,GetTickCount,'List successfuly sent :-)'));
    end else begin
      Result:= HR_ERROR;
      MsgQueue.EnQueue(TbcMessage.Create(LM_ERROR,
                                         aPeerThread.Sock.LastError,
                                         GetTickCount,
                                         aPeerThread.Sock.LastErrorDesc));
    end;
    Free;
  end;
end;

function TfrmMain.SendFile(aPeerThread: TTCPEchoThrd; const aFilename: string): ptrint;
begin
  Result:= HR_ERROR;
  fStream:= TMemoryStream.Create;
  try
    fStream.LoadFromFile('/home/bc/share/'+aFilename);
    fStream.Seek(0,soFromBeginning);
    MsgQueue.EnQueue(TbcMessage.Create(LM_LOADED,fStream.Size,GetTickCount,'/home/bc/share/'+aFilename));
//PostMessage(Handle,LM_CREATE,fStream.Size,ptrint(pchar(' file loaded...')));
    if aPeerThread.Sock.CanWrite(2000) then aPeerThread.Sock.SendStream(fStream);
    if (aPeerThread.Sock.LastError = 0) then begin
      Result:= HR_OK;
      MsgQueue.EnQueue(TbcMessage.Create(LM_SENT,fStream.Size,GetTickCount,'/home/bc/share/'+aFilename+' successfuly sent :-)'));
    end;
//PostMessage(Handle,LM_CREATE,aPeerThread.Sock.LastError,ptrint(pchar(' socket errorcode...')));
  finally FreeAndNil(fStream); end;
end;

{ *** message handlers *** }
procedure TfrmMain.LMCreate(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)))
end;

procedure TfrmMain.LMListen(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMAccept(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMWorking(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMDone(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMDestroy(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;
{ *** message handlers done *** }

procedure TfrmMain.ThreadTerminated(Sender: TObject);
begin
  Caption:= 'Echo daemon is Done!';
end; { *** thread done *** }

procedure TfrmMain.DataInQueue(Sender: TObject);
begin { check message queue with a timer }
  if not Timer1.Enabled then begin
    Timer1.Enabled:= true;
//    ShowToday;
  end;
end;

procedure TfrmMain.CmdHandler(aPeerThread: TTCPEchoThrd;const aCmd: string);
var Filename: string;
begin                {   0       1       2       3       4       5 }
  case StrCase5(aCmd,['.list','.recv','.send','.info','.echo','.quit']) of
    0: begin { send a list of commands to client }
         if SendList(aPeerThread) = HR_ERROR then
           aPeerThread.Sock.SendString(inttostr(ThreadID)+' ERROR! [.info] command received');
       end;
    1: begin
         aPeerThread.Sock.SendString(inttostr(ThreadID)+' [.recv] command received');
       end;
    2: begin { send a file to client }
         Filename:= StringWorkshop.GetFieldToken(2,aCmd,'|');
         if SendFile(aPeerThread,Filename) = HR_ERROR then
           aPeerThread.Sock.SendString(inttostr(ThreadID)+' ERROR! ['+Filename+'] [.send] command received');
       end;
    3: begin
         aPeerThread.Sock.SendString(inttostr(ThreadID)+' [.info] command received');
       end;
    4: begin
         aPeerThread.Sock.SendString(inttostr(ThreadID)+' [.echo] command received');
       end;
    5: begin
         aPeerThread.Sock.SendString(inttostr(ThreadID)+' [.quit] command received');
         aPeerThread.Terminate;
       end;
  end;
end;

end.

