unit uFishFacts;

(* ╔══════════════════════════════════════╗
   ║  FISHFACTS PROJECT                   ║
   ║  ----------------------------------- ║
   ║  Remember that old demo "FishFacts"  ║
   ║  developed with Delphi that displays ║
   ║  a list of fishes.                   ║
   ║  This is the pas2js version          ║
   ║  created by: warleyalex              ║
   ╚══════════════════════════════════════╝ *)
{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  JS, Web, Types, Math, Classes, SysUtils, DB, JSONDataSet, uCDS;

type
  TFishRecord = record
    Category     : string;
    Common_Name  : string;
    Length_Cm    : string;
    Length_In    : string;
    Notes        : string;
    Species_Name : string;
    Species_No   : string;
  end;

type
  JEventListenerHandler = procedure(event: TJSEvent) of Object;

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

type
  { TJFishFacts }
  TJFishFacts = class(TObject)
  private
    (* Private declarations *)
    fConnection: TConnection;
    fDataSet: TDataSet;
    fDataSource: TWDataSource;
    fishRecord: TFishRecord;
    selectedIndex: Integer;
    function JSON2TFishRecord(const Value: TJSObject): TFishRecord;
    function TFishRecord2JSON(const Value: TFishRecord): TJSObject;
    function roundNumber(num: Double; decNumber: integer): string;
    procedure bindEvent(element: TJSElement; EventType: String; handler: JEventListenerHandler);
   // procedure AfterOpenDB(DataSet: TDataSet);
    procedure AfterConn(Sender: TObject);
  protected
    (* Protected declarations *)
    procedure callbackA(e: TJSEvent);
    procedure callbackB(e: TJSEvent);
    procedure callbackC(e: TJSEvent);
    procedure callbackD(e: TJSEvent);
  public
    (* Public declarations *)
    constructor Create;
    procedure downClick(Sender: TObject);
    procedure upClick(Sender: TObject);
    procedure refreshFacts();
    procedure selectionChange();
  published
    (* Published declarations *)
    procedure InitializeObject;
  end;

implementation

var
  body: TJSElement; external name 'document.body';

{ TJFishFacts }

{ ╔══════════════════════════════════════════════════╗
  ║ Insert here the (generated code)  TODO           ║
  ╚══════════════════════════════════════════════════╝ }

constructor TJFishFacts.Create;
var
    i: Longint;
    docFragment : TJSDocumentFragment;
    smsfishananim, smsfishanscene0, div_, smsfishmainscreen, div_0, smsfishon,
    a_, smsfishanobj3, smsfishligar, smsfishanobj5, a_0, smsfishanobj6,
    a_1, smsfishpanelsub, smsfishpeixea, div_1, img_, img_0, smsfishpeixeb,
    div_2, img_1, smsfishcreatedby, smsfishsombra, smsfishpas2js, div_3, img_2,
    smsfishpaneltop, smsfishundersea, div_4, img_3, smsfishfishfacts, div_5,
    img_4, smsfishanobj16, smsfishstatuson, smsfishaboutme, div_6, img_5,
    smsfishhidegrupo, smsfishhidetable, smsfishfishdetails, smsfishpicture,
    div_7, img_6, smsfishmemo, smsfishabout,smsfishcategory, smsfishspeciename,
    smsfishlencm, smsfishlenin : TJSElement;

begin
  { ╔═══════════════════════════════════════════════════════════════════════════╗
    ║ Since the document fragment is in memory and not part of the main DOM     ║
    ║ tree, appending children to it does not cause page reflow (computation    ║
    ║ of elements position and geometry). Consequently, using documentfragments ║
    ║ often results in better performance.                                      ║
    ╚═══════════════════════════════════════════════════════════════════════════╝ }

  docFragment := document.createDocumentFragment(); // contains all gathered nodes
  smsfishananim := document.createElement('DIV');
  smsfishananim.setAttribute('id', 'smsfish-an-anim');
  smsfishananim.setAttribute('id', 'smsfish-an-anim');
  docFragment.appendChild(smsfishananim);

  smsfishanscene0 := document.createElement('DIV');
  smsfishanscene0.setAttribute('id', 'smsfish-an-scene-0');
  smsfishanscene0.setAttribute('class', 'run t-0 paused');
  smsfishananim.appendChild(smsfishanscene0);

  div_ := document.createElement('DIV');
  div_.setAttribute('class', 'smsfish-an-stage');
  smsfishanscene0.appendChild(div_);

  smsfishmainscreen := document.createElement('DIV');
  smsfishmainscreen.setAttribute('id', 'smsfish-mainScreen');
  div_.appendChild(smsfishmainscreen);

  div_0 := document.createElement('DIV');
  smsfishmainscreen.appendChild(div_0);

  img_ := document.createElement('IMG');
  img_.setAttribute('height', '665');
  img_.setAttribute('width', '648');
  img_.setAttribute('src', 'assets/nintendo1.svg');
  div_0.appendChild(img_);

  smsfishon := document.createElement('DIV');
  smsfishon.setAttribute('id', 'smsfish-ON');
  div_.appendChild(smsfishon);

  a_ := document.createElement('BUTTON');
  a_.setAttribute('id', 'smsfish-LIGA');
  a_.setAttribute('data-icon', '|');
  a_.setAttribute('title', 'Turn ON');
  a_.setAttribute('class', 'button green oval icon');
  smsfishon.appendChild(a_);

  smsfishanobj3 := document.createElement('DIV');
  smsfishanobj3.setAttribute('id', 'smsfish-an-obj-3');
  div_.appendChild(smsfishanobj3);

  smsfishligar := document.createElement('DIV');
  smsfishligar.setAttribute('id', 'smsfish-ligar');
  smsfishligar.setAttribute('class', 'nm');
  smsfishanobj3.appendChild(smsfishligar);

  smsfishanobj5 := document.createElement('DIV');
  smsfishanobj5.setAttribute('id', 'smsfish-R');
  smsfishligar.appendChild(smsfishanobj5);

  a_0 := document.createElement('BUTTON');
  a_0.setAttribute('id', 'smsfish-RIGHT');
  a_0.setAttribute('class', 'button pink oval icon');
  a_0.setAttribute('title', 'Love');
  a_0.setAttribute('data-icon', 'R');
  smsfishanobj5.appendChild(a_0);

  smsfishanobj6 := document.createElement('DIV');
  smsfishanobj6.setAttribute('id', 'smsfish-L');
  smsfishligar.appendChild(smsfishanobj6);

  a_1 := document.createElement('BUTTON');
  a_1.setAttribute('id', 'smsfish-LEFT');
  a_1.setAttribute('class', 'button blue oval icon');
  a_1.setAttribute('title', 'Love');
  a_1.setAttribute('data-icon', 'L');
  smsfishanobj6.appendChild(a_1);

  smsfishpanelsub := document.createElement('DIV');
  smsfishpanelsub.setAttribute('id', 'smsfish-panelSub');
  smsfishligar.appendChild(smsfishpanelsub);

  smsfishpeixea := document.createElement('DIV');
  smsfishpeixea.setAttribute('id', 'smsfish-peixeA');
  smsfishpanelsub.appendChild(smsfishpeixea);

  div_1 := document.createElement('DIV');
  smsfishpeixea.appendChild(div_1);

  img_0 := document.createElement('IMG');
  img_0.setAttribute('height', '225');
  img_0.setAttribute('width', '225');
  img_0.setAttribute('src', 'assets/peixeA.png');
  div_1.appendChild(img_0);

  smsfishpeixeb := document.createElement('DIV');
  smsfishpeixeb.setAttribute('id', 'smsfish-peixeB');
  smsfishpanelsub.appendChild(smsfishpeixeb);

  div_2 := document.createElement('DIV');
  smsfishpeixeb.appendChild(div_2);

  img_1 := document.createElement('IMG');
  img_1.setAttribute('height', '225');
  img_1.setAttribute('width', '225');
  img_1.setAttribute('src', 'assets/peixeB.png');
  div_2.appendChild(img_1);

  smsfishcreatedby := document.createElement('DIV');
  smsfishcreatedby.setAttribute('id', 'smsfish-createdby');
  smsfishligar.appendChild(smsfishcreatedby);

  smsfishsombra := document.createElement('DIV');
  smsfishsombra.setAttribute('id', 'smsfish-sombra');
  smsfishcreatedby.appendChild(smsfishsombra);

  smsfishpas2js := document.createElement('DIV');
  smsfishpas2js.setAttribute('id', 'smsfish-pas2js');
  smsfishcreatedby.appendChild(smsfishpas2js);

  div_3 := document.createElement('DIV');
  smsfishpas2js.appendChild(div_3);

  img_2 := document.createElement('IMG');
  img_2.setAttribute('height', '162');
  img_2.setAttribute('width', '404');
  img_2.setAttribute('src', 'assets/pas2js.png');
  div_3.appendChild(img_2);

  smsfishpaneltop := document.createElement('DIV');
  smsfishpaneltop.setAttribute('id', 'smsfish-panelTop');
  smsfishligar.appendChild(smsfishpaneltop);

  smsfishundersea := document.createElement('DIV');
  smsfishundersea.setAttribute('id', 'smsfish-undersea');
  smsfishpaneltop.appendChild(smsfishundersea);

  div_4 := document.createElement('DIV');
  smsfishundersea.appendChild(div_4);

  img_3 := document.createElement('IMG');
  img_3.setAttribute('height', '170');
  img_3.setAttribute('width', '790');
  img_3.setAttribute('src', 'assets/undersea.jpg');
  div_4.appendChild(img_3);

  smsfishfishfacts := document.createElement('DIV');
  smsfishfishfacts.setAttribute('id', 'smsfish-fishfacts');
  smsfishpaneltop.appendChild(smsfishfishfacts);

  div_5 := document.createElement('DIV');
  smsfishfishfacts.appendChild(div_5);

  img_4 := document.createElement('IMG');
  img_4.setAttribute('height', '83');
  img_4.setAttribute('width', '232');
  img_4.setAttribute('src', 'assets/fishfacts.png');
  div_5.appendChild(img_4);

  smsfishanobj16 := document.createElement('DIV');
  smsfishanobj16.setAttribute('id', 'smsfish-an-obj-16');
  smsfishligar.appendChild(smsfishanobj16);

  smsfishstatuson := document.createElement('DIV');
  smsfishstatuson.setAttribute('id', 'smsfish-statusON');
  smsfishanobj16.appendChild(smsfishstatuson);

  smsfishaboutme := document.createElement('DIV');
  smsfishaboutme.setAttribute('id', 'smsfish-aboutMe');
  smsfishanobj16.appendChild(smsfishaboutme);

  div_6 := document.createElement('DIV');
  smsfishaboutme.appendChild(div_6);

  img_5 := document.createElement('IMG');
  img_5.setAttribute('height', '72');
  img_5.setAttribute('width', '75');
  img_5.setAttribute('src', 'assets/tomate.png');
  div_6.appendChild(img_5);

  smsfishhidegrupo := document.createElement('DIV');
  smsfishhidegrupo.setAttribute('id', 'smsfish-hideGrupo');
  smsfishanobj3.appendChild(smsfishhidegrupo);

  smsfishhidetable := document.createElement('DIV');
  smsfishhidetable.setAttribute('id', 'smsfish-hideTable');
  smsfishhidetable.setAttribute('class', 'paused');
  smsfishhidegrupo.appendChild(smsfishhidetable);

  smsfishfishdetails := document.createElement('DIV');
  smsfishfishdetails.setAttribute('id', 'smsfish-fishDetails');
  smsfishfishdetails.setAttribute('class', 'nm');
  div_.appendChild(smsfishfishdetails);

  smsfishpicture := document.createElement('DIV');
  smsfishpicture.setAttribute('id', 'smsfish-picture');
  smsfishfishdetails.appendChild(smsfishpicture);

  div_7 := document.createElement('DIV');
  div_7.setAttribute('style', 'position: initial');
  smsfishpicture.appendChild(div_7);

  img_6 := document.createElement('IMG');
  img_6.setAttribute('height', '100%');
  img_6.setAttribute('width', '100%');
  img_6.setAttribute('src', '');
  div_7.appendChild(img_6);

  smsfishmemo := document.createElement('TEXTAREA');
  smsfishmemo.setAttribute('id', 'smsfish-memo');
  smsfishmemo.setAttribute('style', 'background-color: rgb(188, 188, 222)');
  smsfishfishdetails.appendChild(smsfishmemo);

  smsfishabout := document.createElement('DIV');
  smsfishabout.setAttribute('id', 'smsfish-about');
  smsfishabout.setAttribute('style', 'color: rgb(0, 0, 255); font-size: 20px; text-align: center;');

  smsfishfishdetails.appendChild(smsfishabout);

  smsfishcategory := document.createElement('DIV');
  smsfishcategory.setAttribute('id', 'smsfish-category');
  smsfishcategory.setAttribute('style', 'font-size: 15px; font-weight: bold; color: brown;');
  smsfishfishdetails.appendChild(smsfishcategory);

  smsfishspeciename := document.createElement('DIV');
  smsfishspeciename.setAttribute('id', 'smsfish-specieName');
  smsfishspeciename.setAttribute('style', 'font-size: 15px; font-weight: bold; color: brown;');
  smsfishfishdetails.appendChild(smsfishspeciename);

  smsfishlencm := document.createElement('DIV');
  smsfishlencm.setAttribute('id', 'smsfish-lenCm');
  smsfishlencm.setAttribute('style', 'font-size: 15px; font-weight: bold; color: brown;');
  smsfishfishdetails.appendChild(smsfishlencm);

  smsfishlenin := document.createElement('DIV');
  smsfishlenin.setAttribute('id', 'smsfish-lenIn');
  smsfishlenin.setAttribute('style', 'font-size: 15px; font-weight: bold; color: brown;');
  smsfishfishdetails.appendChild(smsfishlenin);
  body.appendChild( docFragment );

end;

function TJFishFacts.JSON2TFishRecord(const Value: TJSObject): TFishRecord;
begin
  result.Category     := string(Value['Category']);
  result.Common_Name  := string(Value['Common_Name']);
  result.Length_Cm    := string(Value['Length_Cm']);
  result.Length_In    := string(Value['Length_In']);
  result.Notes        := string(Value['Notes']);
  result.Species_Name := string(Value['Species_Name']);
  result.Species_No   := string(Value['Species_No']);
end;

function TJFishFacts.TFishRecord2JSON(const Value: TFishRecord): TJSObject;
begin
  result := TJSObject.New;
  result['Category']     := Value.Category;
  result['Common_Name']  := Value.Common_Name;
  result['Length_Cm']    := Value.Length_Cm;
  result['Length_In']    := Value.Length_In;
  result['Notes']        := Value.Notes;
  result['Species_Name'] := Value.Species_Name;
  result['Species_No']   := Value.Species_No;
end;

function TJFishFacts.roundNumber(num: Double; decNumber: integer): string;
begin
  result := FloatToStr(Round(num * Math.power(10, decNumber)) /
                       Math.power(10, decNumber));
end;

procedure TJFishFacts.selectionChange();
var
  rightBtn, leftBtn, pictureImg, about,
    category, specieName, lenCm, lenIn, memo : TJSElement;

begin
  rightBtn   := document.querySelector('#smsfish-RIGHT');
  leftBtn    := document.querySelector('#smsfish-LEFT');
  pictureImg := document.querySelector('#smsfish-picture img');
  about      := document.querySelector('#smsfish-about');
  category   := document.querySelector('#smsfish-category');
  specieName := document.querySelector('#smsfish-specieName');
  lenCm      := document.querySelector('#smsfish-lenCm');
  lenIn      := document.querySelector('#smsfish-lenIn');
  memo       := document.querySelector('#smsfish-memo');

  (* disable btn "UP" if true *)
  if not ((fDataSet.RecordCount = 0) or (selectedIndex = fDataSet.RecordCount-1)) then
    rightBtn.removeAttribute('disabled')
  else
    rightBtn['disabled'] := 'true';

  (* disable btn "DOWN" if true *)
  if not ((fDataSet.RecordCount = 0) or (selectedIndex = 0)) then
    leftBtn.removeAttribute('disabled')
  else
    leftBtn['disabled'] := 'true';

  (* the fish descriptions values *)
  pictureImg.setAttribute('src', 'pics/' + fishRecord.Species_No +'.png');
  about.innerHTML:= '<b>About the ' + fishRecord.Common_Name + '</b>';
  category.textContent := fishRecord.Category;
  specieName.textContent := fishRecord.Species_Name;
  lenCm.textContent := roundNumber(StrToFloat(fishRecord.Length_Cm), 2);
  lenIn.textContent := roundNumber(StrToFloat(fishRecord.Length_In), 2);
  memo.textContent := fishRecord.Notes;
end;

procedure TJFishFacts.refreshFacts();
begin
 if fDataSet.RecordCount > 0 then
   selectedIndex := 0;
 selectionChange();
end;

procedure TJFishFacts.upClick(Sender: TObject);
begin
if selectedIndex < fDataSet.RecordCount - 1 then
  begin
    Inc(selectedIndex);
    fishRecord := JSON2TFishRecord(
        TJSObject(
          fDataSet.Rows.Elements[selectedIndex]
        )
      );
    selectionChange();
  end;
end;

procedure TJFishFacts.downClick(Sender: TObject);
begin
if (selectedIndex > 0) then
  begin
    Dec(selectedIndex);
    fishRecord := JSON2TFishRecord(
        TJSObject(
          fDataSet.Rows.Elements[selectedIndex]
        )
      );
    selectionChange();
  end;
end;

procedure TJFishFacts.callbackA(e: TJSEvent);
begin
  { after fishDetails animation finishes, just display it }
  document.getElementById('smsfish-fishDetails').classList.remove('nm');
end;

procedure TJFishFacts.callbackB(e: TJSEvent);
var
  btnLiga: TJSElement;

begin
  btnLiga := document.querySelector('#smsfish-LIGA');

  { "Ligar / Turn On / Turn Off button }
  document.getElementById('smsfish-ligar').classList.remove('nm');
  document.getElementById('smsfish-hideTable').classList.remove('paused');

  { Turn ON button now is disabled! }
  btnLiga['disabled'] := 'true';
end;

procedure TJFishFacts.callbackC(e: TJSEvent);
begin
  { "R" Right/Next button }
  upClick(Self);
end;

procedure TJFishFacts.callbackD(e: TJSEvent);
begin
  { "L" Left/Previous button }
  downClick(Self);
end;

procedure TJFishFacts.bindEvent(element: TJSElement; EventType: String;
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

procedure TJFishFacts.AfterConn(Sender: TObject);
begin
  console.log('---------------');
  (* initialize / fill the first TFishRecord *)
  console.log(  fDataset.Active); // TRUE
  fishRecord := JSON2TFishRecord(
     TJSObject(fDataSet.Rows.Elements[0])
  );

  (* update de controls *)
  refreshFacts();
end;

procedure TJFishFacts.InitializeObject;
var
   i, aux: Integer;
   TickStart, TickEnd: Double;
begin
  fConnection:=  TConnection.Create(nil);
  fConnection.Name := 'Connection1';
  fConnection.AfterConnect:= @AfterConn;
  fConnection.Active := false;

  fDataSet:=     TDataSet.Create(nil);
  fDataSet.Name := 'DataSet1';
  fDataSet.Connection := fConnection;

  fDataSource:=  TWDataSource.Create(nil);
  fDataSource.Name := 'DataSource1';
  fDataSource.DataSet := fDataSet;

  fConnection.URI := 'data.json';
  fConnection.DataNode := 'data';
  fDataSet.FieldDefs.Clear();

  fDataSet.FieldDefs.Add('Species_No', TFieldType.ftString,0);
  fDataSet.FieldDefs.Add('Category',  TFieldType.ftString,50);
  fDataSet.FieldDefs.Add('Common_Name', TFieldType.ftString,50);
  fDataSet.FieldDefs.Add('Species_Name', TFieldType.ftString,50);
  fDataSet.FieldDefs.Add('Length__cm_', TFieldType.ftInteger,0);
  fDataSet.FieldDefs.Add('Length_In', TFieldType.ftString,30);
  fDataSet.FieldDefs.Add('Notes', TFieldType.ftString,255);
  fConnection.Active := TRUE;

  TJSHTMLElement(
    document.querySelector('#smsfish-an-scene-0')
  ).style.setProperty('webkitTransition', 'none');

  document.querySelector('#smsfish-an-scene-0').classList.add('paused');

  bindEvent(document.querySelector('#smsfish-hideTable'), 'webkitAnimationEnd mozAnimationEnd MSAnimationEnd oanimationend animationend', @callbackA);
  bindEvent(document.querySelector('#smsfish-ON'), 'click', @callbackB);
  bindEvent(document.querySelector('#smsfish-R'), 'click', @callbackC);
  bindEvent(document.querySelector('#smsfish-L'), 'click', @callbackD);
end;
end.

