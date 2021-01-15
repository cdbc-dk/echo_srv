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
  { TfrmMain }
  TfrmMain = class(TForm)
    btnStartSrv: TButton;
    btnStopSrv: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure btnStartSrvClick(Sender: TObject);
    procedure btnStopSrvClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fEchoSrv: TTCPEchoDaemon;
    fMainThreadId: ptruint;
    fStream: TMemoryStream;
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
procedure ShowToday;
var Dt: IIsoDateTime;
begin
  Dt:= TIsoDateTime.Create;
  ShowMessageFmt('Date- AsInteger: %d, AsString: %s, AsDate: %f, Time- AsInteger: %d, AsString: %s, AsTime: %f',
                 [Dt.Date.AsInteger,Dt.Date.AsString,Dt.Date.AsDate,Dt.Time.AsInteger,Dt.Time.AsString,Dt.Time.AsTime]);
  Dt:= nil;
end;

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
  Caption:= 'Echo Server - [Port: 8008]';
  fMainThreadId:= ThreadID;
  Memo1.Lines.Text:= 'Main Thread Id: '+inttostr(fMainThreadId);
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

function TfrmMain.SendFile(aPeerThread: TTCPEchoThrd; const aFilename: string): ptrint;
begin
  Result:= HR_ERROR;
  fStream:= TMemoryStream.Create;
  try
    fStream.LoadFromFile('/home/bc/y/'+aFilename);
    fStream.Seek(0,soFromBeginning);
    MsgQueue.EnQueue(TbcMessage.Create(LM_LOADED,fStream.Size,GetTickCount,'/home/bc/y/'+aFilename));
//PostMessage(Handle,LM_CREATE,fStream.Size,ptrint(pchar(' file loaded...')));
    if aPeerThread.Sock.CanWrite(2000) then aPeerThread.Sock.SendStream(fStream);
    if (aPeerThread.Sock.LastError = 0) then begin
      Result:= HR_OK;
      MsgQueue.EnQueue(TbcMessage.Create(LM_SENT,fStream.Size,GetTickCount,'/home/bc/y/'+aFilename+' successfuly sent :-)'));
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
begin                {   0      1      2      3      4      5 }
  case StrCase4(aCmd,['.lst','.rcv','.snd','.inf','.ech','.qui']) of
    0: begin
         aPeerThread.Sock.SendString(inttostr(ThreadID)+' [.lst] command received');
       end;
    1: begin
         aPeerThread.Sock.SendString(inttostr(ThreadID)+' [.rcv] command received');
       end;
    2: begin { send a file to client }
         Filename:= StringWorkshop.GetFieldToken(2,aCmd,'|');
         if SendFile(aPeerThread,Filename) = HR_ERROR then
           aPeerThread.Sock.SendString(inttostr(ThreadID)+' ERROR! ['+Filename+'] [.snd] command received');
       end;
    3: begin
         aPeerThread.Sock.SendString(inttostr(ThreadID)+' [.inf] command received');
       end;
    4: begin
         aPeerThread.Sock.SendString(inttostr(ThreadID)+' [.ech] command received');
       end;
    5: begin
         aPeerThread.Sock.SendString(inttostr(ThreadID)+' [.qui] command received');
         aPeerThread.Terminate;
       end;
  end;
end;

end.

