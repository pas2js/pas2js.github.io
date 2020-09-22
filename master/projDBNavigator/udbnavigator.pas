unit uDBNavigator;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, Types, Math, JS, Web, uClient;

type
  { TDBNavigator }
  TDBNavigator = class(TObject)
  private
    (* Private declarations *)
    Client: TClient;
    ListClients : TListClient;
    id, nome, cnpj, ramo: TJSElement;
    firstBtn, priorBtn, nextBtn, lastBtn, addBtn, delBtn,
    editBtn, confirmBtn, cancelBtn, refreshBtn: TJSElement;
  protected
    (* Protected declarations *)
    procedure updateControls(Sender: TJSEvent);
    procedure btnFirstClick(Sender: TJSEvent);
    procedure btnPriorClick(Sender: TJSEvent);
    procedure btnNextClick(Sender: TJSEvent);
    procedure btnLastClick(Sender: TJSEvent);
    procedure btnAddClick(Sender: TJSEvent);
    procedure btnDelClick(Sender: TJSEvent);
    procedure btnEditClick(Sender: TJSEvent);
    procedure btnConfirmClick(Sender: TJSEvent);
    procedure btnCancelClick(Sender: TJSEvent);
    procedure btnRefreshClick(Sender: TJSEvent);
  public
    (* Public declarations *)
    constructor Create;
  published
    (* Published declarations *)
  end;

implementation

type
  JEventListenerHandler = reference to procedure(event: TJSEvent);

type
  JElement = class external name 'Element' (TJSElement)
  Public
    addEventListenerExists: boolean; external name 'addEventListener';
    attachEventExists : boolean; external name 'attachEvent';
    procedure addEventListener(aname: String; callback: JEventListenerHandler; capture : Boolean = false);
    procedure removeEventListener(aname: String; callback: JEventListenerHandler; capture : Boolean = false);
    procedure attachEvent(eventNameWithOn: String; callback: JEventListenerHandler);
    procedure detachEvent(eventNameWithOn: String; callback: JEventListenerHandler);
  end;

{ method attaches an event handler to the specified element
  i.e. bindEvent(document.querySelector('#smsfish-R'), 'click', @callbackC); }
procedure bindEvent(element: TJSElement; EventType: String;
  handler: JEventListenerHandler);
var
  events : TStringDynArray;
   i: Integer;

begin
  events := TJSString(EventType).split(' ');

  (* check if addeventlistener exists / For all major browsers *)
  if (JElement(element).addEventListenerExists) then
  begin
    for i:= 0 to TJSArray(events).length - 1 do
      JElement(element).addEventListener(events[i], handler, false);
  end else
  (* check if attachEvent exists / For IE 8 and earlier *)
  if (JElement(element).attachEventExists) then
  begin
    for i:= 0 to TJSArray(events).length - 1 do
      JElement(element).attachEvent('on'+events[i], handler);
  end;
end;

{ TDBNavigator }

constructor TDBNavigator.Create;
var
  i: Integer;

begin
  id         := document.querySelector('.id');
  nome       := document.querySelector('.nome');
  cnpj       := document.querySelector('.cnpj');
  ramo       := document.querySelector('#exampleFormControlSelect1');
  firstBtn   := document.querySelector('.first');
  priorBtn   := document.querySelector('.prior');
  nextBtn    := document.querySelector('.next');
  lastBtn    := document.querySelector('.last');
  addBtn     := document.querySelector('.add');
  delBtn     := document.querySelector('.del');
  editBtn    := document.querySelector('.edit');
  confirmBtn := document.querySelector('.confirm');
  cancelBtn  := document.querySelector('.cancel');
  refreshBtn := document.querySelector('.refresh');

  // Instantiating Client List
  ListClients := TListClient.Create;

  //By now we're able to handle our list, including, removing and counting. Let's start with Add.
  for i := 1 to 10 do
  begin
    Client := TClient.Create;
    Client.ID        := i+99;//StrToInt(edit1.Text);
    Client.Nome      := 'warleyalex'+IntToStr(i+99);//edit2.Text;
    Client.CNPJ      := '12345'+IntToStr(i+99);//edit3.Text;
    Client.Atividade := TAtividade(RandomRange(1,4)); //Comercio, Servicos, Industria;
    ListClients.Adicionar(Client);
  end;

  { bind some elements }
  bindEvent(firstBtn, 'click', @btnFirstClick);
  bindEvent(priorBtn, 'click', @btnPriorClick);
  bindEvent(nextBtn, 'click', @btnNextClick);
  bindEvent(lastBtn, 'click', @btnLastClick);
  bindEvent(addBtn, 'click', @btnAddClick);
  bindEvent(delBtn, 'click', @btnDelClick);
  bindEvent(editBtn, 'click', @btnEditClick);
  bindEvent(confirmBtn, 'click', @btnConfirmClick);
  bindEvent(cancelBtn, 'click', @btnCancelClick);
  bindEvent(refreshBtn, 'click', @btnRefreshClick);

  { then display the first object when create }
  btnFirstClick(nil);
end;

procedure TDBNavigator.updateControls(Sender: TJSEvent);
begin
  WriteLn('Update Controls');
  TJSHTMLInputElement(id).value   :=  IntToStr(Client.ID);
  TJSHTMLInputElement(nome).value :=  Client.Nome;
  TJSHTMLInputElement(cnpj).value :=  Client.CNPJ;
  TJSHTMLInputElement(ramo).value :=  IntToStr( Ord(Client.Atividade) ); //(Client.Atividade);
  if TJSHTMLInputElement(ramo).value = '2' then
    ramo.setAttribute('style', 'background-color: chartreuse') else
    ramo.setAttribute('style', 'background-color: white');
  if TJSHTMLInputElement(ramo).value = '1' then
    ramo.setAttribute('style', 'background-color: coral');

  TJSHTMLInputElement(nextBtn).disabled := not ListClients.isNextBtnEnabled;
  TJSHTMLInputElement(priorBtn).disabled := not ListClients.isPriorBtnEnabled;

//Client := ListClients.CurrentRecord as TClient; // expected output: this.Client = rtl.as(this.ListClients.CurrentRecord(),$mod.TClient);
  Client := TClient(ListClients.CurrentRecord);   // expected output: this.Client = this.ListClients.CurrentRecord();

  if (not ListClients.isNextBtnEnabled and not ListClients.isPriorBtnEnabled) then
  begin
    TJSHTMLInputElement(id).value   :=  IntToStr(Client.Id);
    TJSHTMLInputElement(nome).value :=  Client.Nome;
    TJSHTMLInputElement(cnpj).value :=  Client.CNPJ;
  end;
end;

procedure TDBNavigator.btnFirstClick(Sender: TJSEvent);
begin
  WriteLn('First record');

  Client := ListClients.FirstRecord as TClient;
  //Client := TClient(ListClients.FirstRecord);
  updateControls(Sender);

end;

procedure TDBNavigator.btnPriorClick(Sender: TJSEvent);
begin
  WriteLn('Previous record');

  //Client := ListClients.PriorRecord as TClient;
  Client := TClient(ListClients.PriorRecord);
  updateControls(Sender);

end;

procedure TDBNavigator.btnNextClick(Sender: TJSEvent);
begin
  WriteLn('Next record');
  Client:= TClient(ListClients.NextRecord); // as TClient;
  updateControls(Sender);
end;

procedure TDBNavigator.btnLastClick(Sender: TJSEvent);
begin
  WriteLn('Last record');
  Client:= ListClients.LastRecord as TClient;
  updateControls(Sender);
end;

procedure TDBNavigator.btnAddClick(Sender: TJSEvent);
begin
  WriteLn('Insert record');

  updateControls(Sender);

  TJSHTMLInputElement(id).value   :=  IntToStr(ListClients.NewRecord+100);
  TJSHTMLInputElement(nome).value :=  '';
  TJSHTMLInputElement(cnpj).value :=  '';
  TJSHTMLInputElement(ramo).value :=  '';
end;

procedure TDBNavigator.btnDelClick(Sender: TJSEvent);
begin
  WriteLn('Delete record');
  ListClients.RemoveCurrentRecord;
  updateControls(Sender);
  { after delete de record go to the previous record }
  btnPriorClick(Sender);
end;

procedure TDBNavigator.btnEditClick(Sender: TJSEvent);
begin
  WriteLn('Edit record');
end;

procedure TDBNavigator.btnConfirmClick(Sender: TJSEvent);
begin
  WriteLn('Confirm Edit record');
  Client := TClient.Create;
  Client.ID        := StrToInt( TJSHTMLInputElement(id).value );
  Client.Nome      := TJSHTMLInputElement(nome).value;
  Client.CNPJ      := TJSHTMLInputElement(cnpj).value;
  Client.Atividade :=  (TAtividade( StrToInt(TJSHTMLInputElement(ramo).value) ));

  ListClients.Adicionar(Client);
  updateControls(Sender);
end;

procedure TDBNavigator.btnCancelClick(Sender: TJSEvent);
begin
  WriteLn('Cancel Edit record');
  btnFirstClick(Sender); // ignore editing
end;

procedure TDBNavigator.btnRefreshClick(Sender: TJSEvent);
begin
  WriteLn('Refresh Edit record');
end;

end.

