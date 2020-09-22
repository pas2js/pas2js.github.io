program project1;

{$mode objfpc}


uses
  Classes, SysUtils, JS, Web, uMain, pas2js.Globals,
  pas2js.Application, Form1, pas2js.Splitter;


(*
uses
  Classes, SysUtils, JS, Web, uMain, pas2js.Globals,
  pas2js.Form, pas2js.Application, pas2js.Image, pas2js.ToolBar,
  pas2js.ListBox, pas2js.ProgressBar, pas2js.TextArea,
  pas2js.Video, pas2js.Anchor, pas2js.Loader, pas2js.Iframe,
  pas2js.StreetView, pas2js.Grid, pas2js.ObjectTable, pas2js.StringTable,
  pas2js.Canvas, pas2js.TreeView, pas2js.CheckBox, pas2js.Spinner,
  pas2js.FieldSet, pas2js.RadioButton, pas2js.Input,
  pas2js.FlipScroll, pas2js.Dialog, pas2js.Window,
  pas2js.Panel, pas2js.Button, pas2js.Select, Form1;

type
  TApp = class
    private
      Fpanel1 : TWPanel;
      Fbutton1: TWButton;
    public
      procedure panel1Click(Sender: TObject);
      procedure button1Click(Sender: TObject);
  end;

procedure TApp.panel1Click(Sender: TObject);
begin
  console.log('clicked silver item #' + (Sender as TWPanel).tag);
end;

procedure TApp.button1Click(Sender:TObject);
begin
  console.log('button1 clicked');
end;


var
  i: integer;
  app: TApp;
*)

begin
  // Your code here
(*
  app:= TApp.Create;
  try
    with app do
    begin
      Fpanel1 := TWPanel.Create(nil);
      Fpanel1.setProperty('background-color', 'silver');
      Fpanel1.SetBounds(5, 10, 100, 35);
      Fpanel1.tag := IntToStr(i);
      Fpanel1.OnClick := @panel1Click;

      Fbutton1 := TWButton.Create(nil);
      Fbutton1.SetBounds(5, 50, 100, 50);
      Fbutton1.SetInnerHTML('Button1');
      Fbutton1.OnClick := @button1Click;
    end;
  finally
    app.free;
  end;
*)
  //create forms
  Application.CreateForm('Form1',TForm1);
  (*Application.CreateForm('Form2',TForm2);
  Application.CreateForm('Form3',TForm3);
  Application.CreateForm('Form4',TForm4);
  Application.CreateForm('Form5',TForm5);
  Application.CreateForm('Form6',TForm6);
  Application.CreateForm('Form7',TForm7);
  Application.CreateForm('Form8',TForm8);
  Application.CreateForm('Form9',TForm9);
  Application.CreateForm('Form10',TForm10);*)

  //show initial form
  Application.GoToForm('Form1');
end.
