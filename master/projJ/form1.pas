unit Form1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web,
  pas2js.Element, pas2js.Form, pas2js.ListBox, pas2js.Panel, pas2js.Button, pas2js.Image, pas2js.ToolBar, pas2js.ProgressBar, pas2js.Splitter, 
  pas2js.Select, pas2js.Video, pas2js.Anchor, pas2js.TextArea, pas2js.IFrame, pas2js.FlipScroll, pas2js.Loader, pas2js.Spinner, pas2js.Grid, 
  pas2js.Canvas, pas2js.TreeView, pas2js.CheckBox, pas2js.FieldSet, 
  pas2js.RadioButton, pas2js.Window, pas2js.Dialog, pas2js.Input;

type
  TProcedure = procedure of object;

type
  TComponentRec = class
    name : string;
    ShowIt : TProcedure;
  end;


type
  TForm1 = class(TWForm)
  private
    ToolBar : TWToolBar;
    ListBox1 : TWListBox;
    Components : array of TComponentRec;
    DisplayDiv : TWPanel;
    ComponentRec: TComponentRec;
    procedure populateListBox;
  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;
  end;

implementation

procedure TForm1.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
end;


procedure TForm1.populateListBox;
var
  i: Integer;
  Panel0: TWPanel;

  procedure doPanel0OnClick(Sender: TObject);
  var
    disp: TJSCSSStyleDeclaration;
  begin
    //clear DisplayDiv
    While assigned(DisplayDiv.Handle.firstChild) do
      DisplayDiv.Handle.removeChild(DisplayDiv.Handle.firstChild);
    //optionally clear select element, see JInput
    if document.getElementById(self.Handle.getAttribute('data-select')) <> null then
      TJSHTMLElement(document.getElementById(self.Handle.getAttribute('data-select'))).style.setProperty('display' ,'none');
    //execute component
    Components[strtoint((Sender as TWPanel).tag)].ShowIt;
    //adjust height and width of DisplayDiv to componentheight and -width
    disp := TJSHTMLElement(DisplayDiv.Handle.children[0]).style;
    disp.setProperty('height', 'px');

    //DisplayDiv.Height := StrToInt(StrBefore(DisplayDiv.Handle).children[0].style.height, 'px')) + 30;
    //DisplayDiv.Width  := StrToInt(StrBefore(DisplayDiv.Handle).children[0].style.width, 'px')) + 30;

    //TCustomControl.SetHeight(Self.DisplayDiv,parseInt(StrBefore(String(Self.DisplayDiv.Handle.children[0].style.height),"px"),10)+30);
    //TCustomControl.SetWidth(Self.DisplayDiv,parseInt(StrBefore(String(Self.DisplayDiv.Handle.children[0].style.width),"px"),10)+30);
    DisplayDiv.Height :=
    StrToInt(
    StrBefore(
      TJSHTMLElement(DisplayDiv.Handle.children[0])
      .style.getPropertyValue('height'), 'px')
    ) + 30;

    DisplayDiv.Width :=
    StrToInt(
      StrBefore(
        TJSHTMLElement(DisplayDiv.Handle.children[0])
        .style.getPropertyValue('width'), 'px')
    ) + 30;

//    DisplayDiv.Height := 250;
//    DisplayDiv.Width := 300;
   DisplayDiv.Top := 280;
  end;

begin
  ///////////////////////////////////////////////////////
  //
  //populate the menu (listbox) with all component names
  //when clicked, execute the saved ShowIt procedure
  //
    ListBox1 := TWListBox.Create(self);
    ListBox1.SetBounds(17, 85, 200, 170);
    ListBox1.setProperty('background-color', 'white');
    ListBox1.SetProperty('border','2px double whitesmoke');

    For i := 0 to TJSArray(Components).Length -1 do begin
      Panel0 := TWPanel.Create(ListBox1);
      Panel0.setProperty('background-color', 'whitesmoke');
      Panel0.SetBounds(2,2,170,22);
      Panel0.SetinnerHTML(Components[i].Name);
      Panel0.SetProperty('font-size', '0.85em');
      Panel0.tag := inttostr(i);
      Panel0.OnClick := @doPanel0OnClick;

      ListBox1.Add(Panel0);
    end;

end;


procedure TForm1.InitializeObject;
var
  i: Integer;
  Panel0, Panel1: TWPanel;
  Fpanel1: TWPanel;
  Fbutton1: TWButton;
  Image0: TWImage;

  procedure doPanel0OnClick(Sender: TObject);
  begin
    console.log('clicked silver item #' + (Sender as TWPanel).tag);
  end;
  procedure doPanel1OnClick(Sender: TObject);
  begin
    //console.log('clicked green item #' + (Sender as TWPanel).tag);
    console.log('button clicked');
  end;

  procedure doPanel1;
  var
    Panel1: TWPanel;
  begin
      {--------------------------------------------------------------------------}
      { Component  TWPanel                                                      }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Panel1 := TWPanel.Create(DisplayDiv);
      Panel1.SetBounds(0, 0, 100, 100);
      Panel1.setProperty('background-color', 'gold');
      Panel1.SetProperty('border','1px double grey');
    end;

  procedure doButton1;
  var
    Button1 : TWButton;
    procedure Button1OnClick(sender: TObject);
      begin
        window.alert('clicked');
      end;
  begin
      {--------------------------------------------------------------------------}
      { Component  TWButton                                                     }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Button1 := TWButton.Create(DisplayDiv);
      Button1.SetBounds(0, 0, 100, 50);
      Button1.SetInnerHTML('Button');
      Button1.OnClick := @Button1OnClick;
    end;

  procedure doProgress1;
  var
    Progress1 : TWProgress;
    id: Integer;

    procedure ProgressIt;
    begin
      Progress1.Perc := Progress1.Perc + 1;
      if Progress1.Perc > 100 then
        window.clearInterval(id);
    end;

  begin
      {------------------------------------------------------------------------------}
      { Component  TWProgress                                                       }
      { Updated:   01/09/2017                                                        }
      {------------------------------------------------------------------------------}
      Progress1 := TWProgress.Create(DisplayDiv);
      Progress1.SetBounds(0, 0, 300, 12);
      Progress1.setProperty('background-color', 'lightgrey');
      Progress1.ProgressBar.setProperty('background-color', 'salmon');
      Progress1.Perc := 25;
      id := window.setInterval(@ProgressIt, 30);
    end;

    procedure doImage1;
    var
      Image1 : TWImage;
    begin
        {--------------------------------------------------------------------------}
        { Component  TWImage                                                      }
        { Updated:   01/09/2017                                                    }
        {--------------------------------------------------------------------------}
        Image1 := TWImage.Create(DisplayDiv);
        Image1.SetBounds(0, 0, 194, 45);
        Image1.setAttribute('src','images/logo.png');
    end;

    procedure doSplitter1;
    var
      Splitter1 : TWSplitter;
      Button1 : TWButton;
      Button2 : TWButton;
    begin
        {--------------------------------------------------------------------------}
        { Component  TWSplitter                                                   }
        { Updated:   01/09/2017                                                    }
        {--------------------------------------------------------------------------}
        Splitter1 := TWSplitter.Create(DisplayDiv);
        Splitter1.SetBounds(0, 0, 300, 200);
        Splitter1.PanelLeft.setProperty('background-color', 'white');
        Splitter1.PanelRight.setProperty('background-color', 'whitesmoke');
        Splitter1.SetProperty('border','1px solid silver');

        Button1 := TWButton.Create(Splitter1.PanelLeft);
        Button1.SetinnerHTML('Left');
        Button1.SetBounds(20,20,60,30);
        Button2 := TWButton.Create(Splitter1.PanelRight);
        Button2.SetinnerHTML('Right');
        Button2.SetBounds(20,20,60,30);
      end;

    procedure doJToolBar1;
    var
      ToolBar1: TWToolBar;
    begin
      {--------------------------------------------------------------------------}
      { Component  TWToolBar                                                    }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      ToolBar1 := TWToolBar.Create(DisplayDiv);
      ToolBar1.SetBounds(0, 0, 500, 40);
      ToolBar1.setProperty('background-color', '#699BCE');
      ToolBar1.AddMenu('Fish-Facts','Form3','white');
      ToolBar1.AddMenu('VR image',  'Form4','white');
      ToolBar1.AddMenu('StreetView','Form5','white');
    end;

    procedure doJSelect1;
    var
      Select1: TWSelect;
      Item1: TWPanel;
      i: Integer;
    begin
      {--------------------------------------------------------------------------}
      { Component  TWSelect (combobox)                                          }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Select1 := TWSelect.Create(DisplayDiv);
      Select1.SetBounds(0, 0, 200, 200);
      Select1.setProperty('background-color', 'white');
      For i := 1 to 15 do begin
        Item1 := TWPanel.Create(Select1);
        Item1.setProperty('background-color', 'whitesmoke');
        Item1.Height := 20;
        Item1.SetinnerHTML('Item ' + IntToStr(i));
        Item1.tag := 'Item ' + inttostr(i);
        Select1.Add(Item1);
      end;
    end;

    procedure doJListBox1;
    var
      Panel0: TWPanel;
      Colours: Array of String;
      i: Integer;

    begin
      {--------------------------------------------------------------------------}
      { Component  TWListBox                                                    }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      ListBox1 := TWListBox.Create(DisplayDiv);
      ListBox1.SetBounds(0, 0, 200, 1000);
      ListBox1.setProperty('background-color', 'white');
      ListBox1.SetProperty('border','2px double whitesmoke');

      //Colours.Clear;
      TJSArray(Colours).Length := 0; // Colours.Clear;
      TJSArray(Colours).push('White');
      TJSArray(Colours).Push('AliceBlue');
      TJSArray(Colours).Push('AntiqueWhite');
      TJSArray(Colours).Push('Aqua');
      TJSArray(Colours).Push('Aquamarine');
      TJSArray(Colours).Push('Azure');
      TJSArray(Colours).Push('Beige');
      TJSArray(Colours).Push('Bisque');
      TJSArray(Colours).Push('Black');
      TJSArray(Colours).Push('BlanchedAlmond');
      TJSArray(Colours).Push('Blue');
      TJSArray(Colours).Push('BlueViolet');
      TJSArray(Colours).Push('Brown');
      TJSArray(Colours).Push('BurlyWood');
      TJSArray(Colours).Push('CadetBlue');
      TJSArray(Colours).Push('Chartreuse');
      TJSArray(Colours).Push('Chocolate');
      TJSArray(Colours).Push('Coral');
      TJSArray(Colours).Push('CornflowerBlue');
      TJSArray(Colours).Push('Cornsilk');
      TJSArray(Colours).Push('Crimson');
      TJSArray(Colours).Push('Cyan');
      TJSArray(Colours).Push('DarkBlue');
      TJSArray(Colours).Push('DarkCyan');
      TJSArray(Colours).Push('DarkGoldenRod');
      TJSArray(Colours).Push('DarkGray');
      TJSArray(Colours).Push('DarkGrey');
      TJSArray(Colours).Push('DarkGreen');
      TJSArray(Colours).Push('DarkKhaki');
      TJSArray(Colours).Push('DarkMagenta');
      TJSArray(Colours).Push('DarkOliveGreen');
      TJSArray(Colours).Push('Darkorange');
      TJSArray(Colours).Push('DarkOrchid');
      TJSArray(Colours).Push('DarkRed');
      TJSArray(Colours).Push('DarkSalmon');
      TJSArray(Colours).Push('DarkSeaGreen');
      TJSArray(Colours).Push('DarkSlateBlue');
      TJSArray(Colours).Push('DarkSlateGray');
      TJSArray(Colours).Push('DarkSlateGrey');
      TJSArray(Colours).Push('DarkTurquoise');
      TJSArray(Colours).Push('DarkViolet');
      TJSArray(Colours).Push('DeepPink');
      TJSArray(Colours).Push('DeepSkyBlue');
      TJSArray(Colours).Push('DimGray');
      TJSArray(Colours).Push('DimGrey');
      TJSArray(Colours).Push('DodgerBlue');
      TJSArray(Colours).Push('FireBrick');
      TJSArray(Colours).Push('FloralWhite');
      TJSArray(Colours).Push('ForestGreen');
      TJSArray(Colours).Push('Fuchsia');
      TJSArray(Colours).Push('Gainsboro');
      TJSArray(Colours).Push('GhostWhite');
      TJSArray(Colours).Push('Gold');
      TJSArray(Colours).Push('GoldenRod');
      TJSArray(Colours).Push('Gray');
      TJSArray(Colours).Push('Grey');
      TJSArray(Colours).Push('Green');
      TJSArray(Colours).Push('GreenYellow');
      TJSArray(Colours).Push('HoneyDew');
      TJSArray(Colours).Push('HotPink');
      TJSArray(Colours).Push('IndianRed');
      TJSArray(Colours).Push('Indigo');
      TJSArray(Colours).Push('Ivory');
      TJSArray(Colours).Push('Khaki');
      TJSArray(Colours).Push('Lavender');
      TJSArray(Colours).Push('LavenderBlush');
      TJSArray(Colours).Push('LawnGreen');
      TJSArray(Colours).Push('LemonChiffon');
      TJSArray(Colours).Push('LightBlue');
      TJSArray(Colours).Push('LightCoral');
      TJSArray(Colours).Push('LightCyan');
      TJSArray(Colours).Push('LightGoldenRodYellow');
      TJSArray(Colours).Push('LightGray');
      TJSArray(Colours).Push('LightGrey');
      TJSArray(Colours).Push('LightGreen');
      TJSArray(Colours).Push('LightPink');
      TJSArray(Colours).Push('LightSalmon');
      TJSArray(Colours).Push('LightSeaGreen');
      TJSArray(Colours).Push('LightSkyBlue');
      TJSArray(Colours).Push('LightSlateGray');
      TJSArray(Colours).Push('LightSlateGrey');
      TJSArray(Colours).Push('LightSteelBlue');
      TJSArray(Colours).Push('LightYellow');
      TJSArray(Colours).Push('Lime');
      TJSArray(Colours).Push('LimeGreen');
      TJSArray(Colours).Push('Linen');
      TJSArray(Colours).Push('Magenta');
      TJSArray(Colours).Push('Maroon');
      TJSArray(Colours).Push('MediumAquaMarine');
      TJSArray(Colours).Push('MediumBlue');
      TJSArray(Colours).Push('MediumOrchid');
      TJSArray(Colours).Push('MediumPurple');
      TJSArray(Colours).Push('MediumSeaGreen');
      TJSArray(Colours).Push('MediumSlateBlue');
      TJSArray(Colours).Push('MediumSpringGreen');
      TJSArray(Colours).Push('MediumTurquoise');
      TJSArray(Colours).Push('MediumVioletRed');
      TJSArray(Colours).Push('MidnightBlue');
      TJSArray(Colours).Push('MintCream');
      TJSArray(Colours).Push('MistyRose');
      TJSArray(Colours).Push('Moccasin');
      TJSArray(Colours).Push('NavajoWhite');
      TJSArray(Colours).Push('Navy');
      TJSArray(Colours).Push('OldLace');
      TJSArray(Colours).Push('Olive');
      TJSArray(Colours).Push('OliveDrab');
      TJSArray(Colours).Push('Orange');
      TJSArray(Colours).Push('OrangeRed');
      TJSArray(Colours).Push('Orchid');
      TJSArray(Colours).Push('PaleGoldenRod');
      TJSArray(Colours).Push('PaleGreen');
      TJSArray(Colours).Push('PaleTurquoise');
      TJSArray(Colours).Push('PaleVioletRed');
      TJSArray(Colours).Push('PapayaWhip');
      TJSArray(Colours).Push('PeachPuff');
      TJSArray(Colours).Push('Peru');
      TJSArray(Colours).Push('Pink');
      TJSArray(Colours).Push('Plum');
      TJSArray(Colours).Push('PowderBlue');
      TJSArray(Colours).Push('Purple');
      TJSArray(Colours).Push('Red');
      TJSArray(Colours).Push('RosyBrown');
      TJSArray(Colours).Push('RoyalBlue');
      TJSArray(Colours).Push('SaddleBrown');
      TJSArray(Colours).Push('Salmon');
      TJSArray(Colours).Push('SandyBrown');
      TJSArray(Colours).Push('SeaGreen');
      TJSArray(Colours).Push('SeaShell');
      TJSArray(Colours).Push('Sienna');
      TJSArray(Colours).Push('Silver');
      TJSArray(Colours).Push('SkyBlue');
      TJSArray(Colours).Push('SlateBlue');
      TJSArray(Colours).Push('SlateGray');
      TJSArray(Colours).Push('SlateGrey');
      TJSArray(Colours).Push('Snow');
      TJSArray(Colours).Push('SpringGreen');
      TJSArray(Colours).Push('SteelBlue');
      TJSArray(Colours).Push('Tan');
      TJSArray(Colours).Push('Teal');
      TJSArray(Colours).Push('Thistle');
      TJSArray(Colours).Push('Tomato');
      TJSArray(Colours).Push('Turquoise');
      TJSArray(Colours).Push('Violet');
      TJSArray(Colours).Push('Wheat');
      TJSArray(Colours).Push('WhiteSmoke');
      TJSArray(Colours).Push('Yellow');
      TJSArray(Colours).Push('YellowGreen');

      For i := 0 to TJSArray(Colours).Length - 1 do begin
        Panel0 := TWPanel.Create(ListBox1);
        Panel0.setProperty('background-color', Colours[i]);
        Panel0.SetinnerHTML(Colours[i]);
        Panel0.SetProperty('font-size','0.85em');
        Panel0.SetBounds(2,2,170,26);
  //
        ListBox1.Add(Panel0);
      end;
    end;

    procedure doJVideo1;
    var
      Video1: TWVideo;
    begin
      {--------------------------------------------------------------------------}
      { Component  TWVideo                                                      }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Video1 := TWVideo.Create(DisplayDiv);
      Video1.SetBounds(0, 0, 400, 300);
      Video1.setAttribute('src','videos/looprake.mp4');
      Video1.SetAttribute('type','video/mp4');
      Video1.SetAttribute('controls','true');
  //    Video1.SetAttribute('autoplay','true');
      Video1.setProperty('width', '400px');
      Video1.setProperty('height', '300px');
      Video1.SetProperty('object-fit','fill');
    end;

    procedure doJAnchor1;
    var
      Anchor1: TWAnchor;
    begin
      {--------------------------------------------------------------------------}
      { Component  TWAnchor (url link)                                          }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Anchor1 := TWAnchor.Create(DisplayDiv);
      Anchor1.SetBounds(0, 0, 194, 45);
      Anchor1.setAttribute('href','https://www.youtube.com/watch?v=9ehsFrakgAo');
      Anchor1.setAttribute('target','_blank');
      Anchor1.placeholder.SetAttribute('src','images/logo.png');
      Anchor1.placeholder.SetAttribute('alt','LynkFS logo');
      Anchor1.placeholder.SetBounds(0,0,Anchor1.width,Anchor1.height);
    end;

    procedure doJTextArea1;
    var
      Memo1: TWTextArea;
    begin
      {--------------------------------------------------------------------------}
      { Component  TWTextArea (memo)                                            }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Memo1 := TWTextArea.Create(DisplayDiv);
      Memo1.SetBounds(0, 0, 300, 100);
      Memo1.setProperty('background-color', 'whitesmoke');
      Memo1.SetProperty('border','1px double grey');
      Memo1.SetInnerHTML('Lorem ipsum dolor sit amet, consectetur adipiscing elit. ' +
                         'Vestibulum a ipsum leo. Vestibulum a ante ipsum primis in faucibus ' +
                         'orci luctus et ultrices posuere cubilia Curae; Phasellus tincidunt ' +
                         'pretium enim, mollis finibus lacus aliquam sed. Sed molestie mi eu ' +
                         'rhoncus aliquet. Ut ac aliquam quam. Pellentesque at vulputate urna.');

    end;

    procedure doJIFrame1;
    var
      IFrame1: TWIFrame;
    begin
      {--------------------------------------------------------------------------}
      { Component  TWIFrame                                                     }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      IFrame1 := TWIFrame.Create(DisplayDiv);
      IFrame1.SetBounds(0, 0, 650, 500);
      IFrame1.setAttribute('src','http://pas2js.38893.n8.nabble.com');
    end;

    procedure doJFlipPage1;
    var
      FlipScroll: TWFlipScroll;
      encodestr: String;
      ClickPanel: TWPanel;
      PageNr: Integer;

      procedure ClickPanelOnClick(Sender: TObject);
      begin
        Inc(PageNr);
        //FlipScroll.GotoPage(Inc(PageNr));
        FlipScroll.GotoPage(PageNr);

      end;

    begin
      {--------------------------------------------------------------------------}
      { Component  TWFlipScroll                                                 }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      FlipScroll := TWFlipScroll.Create(DisplayDiv);
      FlipScroll.SetBounds(0, 0, 450, 450);

      encodestr := encodeURIComponent('http://www.symphonyone.com/');
      //FlipScroll.setAttribute('src','http://www.lynkit.com.au/native/www/lib/pageflip.php?url=' + encodestr);
      FlipScroll.setAttribute('src','https://rawcdn.githack.com/pas2js/master/master/projATM/www/index.html');

      ClickPanel := TWPanel.Create(DisplayDiv);
      ClickPanel.SetBounds(0,0,FlipScroll.width,FlipScroll.height);
      ClickPanel.SetProperty('opacity','0');

      PageNr := 0;
      ClickPanel.OnClick := @ClickPanelOnClick;

    end;

    procedure doJLoader1;
    var
      Loader1 : TWLoader;
    begin
      {--------------------------------------------------------------------------}
      { Component  TWLoader (spinner)                                           }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Loader1 := TWLoader.Create(DisplayDiv);
      Loader1.SetBounds(0, 0, 60, 60);
    end;

    procedure doJSpinner1;
    var
        Spinner1: TWSpinner;
    begin
      {--------------------------------------------------------------------------}
      { Component  TWSpinner                                                    }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Spinner1 := TWSpinner.Create(DisplayDiv);
      Spinner1.SetBounds(20, 0, 40, 40);
    end;

    procedure doJGrid1;
    var
      Grid1: TWGrid;
      row, column: integer;
      S: String;
      CellPnl: TWPanel;
      CellImg: TWImage;

      procedure CellPnlOnClick(Sender: TObject);
      begin
        window.alert((sender as TCustomControl).tag);
      end;

      procedure CellImgOnClick(Sender: TObject);
      begin
        window.alert((sender as TCustomControl).tag);
      end;


    begin
      {--------------------------------------------------------------------------}
      { Component  TWGrid (listbox based)                                       }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Grid1 := TWGrid.Create(DisplayDiv);
      Grid1.SetBounds(0, 0, 330, 250);

      Grid1.CanResize := true;          //adds column resizing

  //  use case : make a grid with 3 columns and 300 rows.
  //  Columns 1 and 3 contain text and column 2 contains an image.
  //  Different widths and heights as well

      Grid1.AddColumn('Col 1',74);      //title, width
      Grid1.AddColumn('Col 2',134);
      Grid1.AddColumn('Col 3',84);


  //  Grid1.AddCell(row, column, content)
      for row := 1 to 300 do begin
        For column := 1 to 3 do begin
          S := 'Cell ' + inttostr(row) + '-' + inttostr(column);

          case column of
            1,3 :
              begin
                CellPnl := TWPanel.Create(Grid1);
                CellPnl.SetinnerHTML(S);
                CellPnl.Height := 24;
                CellPnl.SetProperty('font-size', '0.85em');
                CellPnl.tag := S;
                CellPnl.OnClick := @CellPnlOnClick;
                Grid1.AddCell(row, column, CellPnl);
              end;
            2: begin
                CellImg := TWImage.Create(Grid1);
                CellImg.setAttribute('src','images/logo.png');
                CellImg.Height := 45;
                CellImg.tag := S;
                CellImg.OnClick := @CellImgOnClick;
                Grid1.AddCell(row, column, CellImg);
               end;
          end;
        end;
      end;
    end;

    procedure doJCanvas1;
    var
      Canvas1: TWCanvas;
    begin
      {--------------------------------------------------------------------------}
      { Component  TWCanvas                                                     }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Canvas1 := TWCanvas.Create(DisplayDiv);
      Canvas1.SetBounds(0, 0, 400, 200);

      // Quadratric curves on 2d-context (ctx)
      Canvas1.ctx.beginPath();
      Canvas1.ctx.moveTo(75, 25);
      Canvas1.ctx.quadraticCurveTo(25, 25, 25, 62.5);
      Canvas1.ctx.quadraticCurveTo(25, 100, 50, 100);
      Canvas1.ctx.quadraticCurveTo(50, 120, 30, 125);
      Canvas1.ctx.quadraticCurveTo(60, 120, 65, 100);
      Canvas1.ctx.quadraticCurveTo(125, 100, 125, 62.5);
      Canvas1.ctx.quadraticCurveTo(125, 25, 75, 25);
      Canvas1.ctx.stroke();
    end;

    procedure doJTreeView1;
    var
      TreeView1: TWTreeView;

    begin
      {--------------------------------------------------------------------------}
      { Component  TWTreeView                                                   }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      TreeView1 := TWTreeView.Create(DisplayDiv);
      TreeView1.SetBounds(0, 0, 250, 200);
      TreeView1.setProperty('background-color', 'white');
      TreeView1.Subject := 'Job roles';

      TreeView1.Add('ceo','','chief executive officer');       //root
      TreeView1.Add('cto', 'ceo','chief technology officer');
      TreeView1.Add('dev1', 'cto','developer 1');
      TreeView1.Add('dev2', 'cto','developer 2');
      TreeView1.Add('dev3', 'cto','developer 3');
      TreeView1.Add('assistent', 'dev2','assistant developer 2');
      TreeView1.Add('cfo', 'ceo','chief financial officer');
      TreeView1.Add('accountant', 'cfo','bean counter');
      TreeView1.Add('cmo', 'ceo','chief marketing officer');
    end;

    procedure doJCheckBox1;
    var
      CheckBox1: TWCheckBox;

    begin
      {--------------------------------------------------------------------------}
      { Component  TWCheckBox                                                   }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      CheckBox1 := TWCheckBox.Create(DisplayDiv);
      CheckBox1.SetBounds(0, 0, 200, 200);

      CheckBox1.&Label := 'First and only checkbox';
      CheckBox1.Checked := true;

      CheckBox1.CheckBoxDimension := 20;      //default
    end;

    procedure doJCheckBox2;
    var
      FieldSet: TWFieldSet;
      CheckBox1: TWCheckBox;
      CheckBox2: TWCheckBox;
      CheckBox3: TWCheckBox;
    begin
      {--------------------------------------------------------------------------}
      { Component  TWCheckBox                                                   }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      FieldSet := TWFieldSet.Create(DisplayDiv);
      FieldSet.SetBounds(0, 0, 200, 180);
      FieldSet.Legend := 'Legend';

      //no need for CheckBoxes.SetBounds()
      CheckBox1 := TWCheckBox.Create(FieldSet);
      CheckBox1.&Label := 'First checkbox';
      CheckBox1.Checked := true;

      CheckBox2 := TWCheckBox.Create(FieldSet);
      CheckBox2.&Label := 'Second checkbox';
      CheckBox2.Checked := false;

      CheckBox3 := TWCheckBox.Create(FieldSet);
      CheckBox3.&Label := 'Third checkbox';
      CheckBox3.Checked := true;
    end;

    procedure doJRadioButton1;
    var
      FieldSet: TWFieldSet;
      RadioButton1: TWRadioButton;
      RadioButton2: TWRadioButton;
      RadioButton3: TWRadioButton;

    begin
      {--------------------------------------------------------------------------}
      { Component  TWRadioButton                                                }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      FieldSet := TWFieldSet.Create(DisplayDiv);
      FieldSet.SetBounds(0, 0, 200, 180);
      FieldSet.Legend := 'Legend';

      //no need for RadioButtones.SetBounds()
      RadioButton1 := TWRadioButton.Create(FieldSet);
      RadioButton1.&Label := 'First RadioButton';

      RadioButton2 := TWRadioButton.Create(FieldSet);
      RadioButton2.&Label := 'Second RadioButton';
      RadioButton2.Checked := true;

      RadioButton3 := TWRadioButton.Create(FieldSet);
      RadioButton3.&Label := 'Third RadioButton';
    end;

    procedure doTWindow1;
    var
      Window1: TWWindow;
      MyButton: TWButton;
      Button1: TWButton;

      procedure doMyButtonOnClick(Sender: TObject);
      var
        FieldSet: TWFieldSet;
        RadioButton1: TWRadioButton;
        RadioButton2: TWRadioButton;
        RadioButton3: TWRadioButton;

        procedure doButton1OnClick(Sender: TObject);
        begin
          TJSHTMLElement(RadioButton1.Handle).click();
        end;

      begin
        FieldSet := TWFieldSet.Create(Window1);
        FieldSet.SetBounds(10, 0, 200, 140);
        FieldSet.Legend := 'Legend';

        RadioButton1 := TWRadioButton.Create(FieldSet);
        RadioButton1.&Label := 'First RadioButton';

        RadioButton2 := TWRadioButton.Create(FieldSet);
        RadioButton2.&Label := 'Second RadioButton';
        RadioButton2.Checked := true;

        RadioButton3 := TWRadioButton.Create(FieldSet);
        RadioButton3.&Label := 'Third RadioButton';

        Button1 := TWButton.Create(Window1);
        Button1.SetBounds(15,200,150,40);
        Button1.SetInnerHTML('check first radiobutton');
        Button1.OnClick := @doButton1OnClick;

        Window1.OpenWindow;
      end;


    begin
      {--------------------------------------------------------------------------}
      { Component  TWWindow                                                     }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Window1 := TWWindow.Create(self); //or application, or nil
      MyButton := TWButton.Create(DisplayDiv);
      MyButton.SetBounds(0,0,150,40);
      MyButton.SetInnerHTML('Open window');
      MyButton.setProperty('background-color', 'blueviolet');
      MyButton.OnClick := @doMyButtonOnClick;

    end;

    procedure doJDialog1;
    var
      Dialog1: TWDialog;
      MyButton: TWButton;

      procedure doMyButtonOnClick(Sender: TObject);
      var
        Canvas1: TWCanvas;
      begin
        Canvas1 := TWCanvas.Create(Dialog1);
        Canvas1.SetBounds(0, 0, 400, 200);
        Canvas1.ctx.beginPath();
        Canvas1.ctx.moveTo(75, 25);
        Canvas1.ctx.quadraticCurveTo(25, 25, 25, 62.5);
        Canvas1.ctx.quadraticCurveTo(25, 100, 50, 100);
        Canvas1.ctx.quadraticCurveTo(50, 120, 30, 125);
        Canvas1.ctx.quadraticCurveTo(60, 120, 65, 100);
        Canvas1.ctx.quadraticCurveTo(125, 100, 125, 62.5);
        Canvas1.ctx.quadraticCurveTo(125, 25, 75, 25);
        Canvas1.ctx.stroke();
        Dialog1.OpenDialog('testing...');
      end;

    Begin
      {--------------------------------------------------------------------------}
      { Component  TWDialog                                                     }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Dialog1 := TWDialog.Create(self); //or application, or form

      MyButton := TWButton.Create(DisplayDiv);

      MyButton.SetBounds(0,0,150,40);
      MyButton.SetInnerHTML('Open dialog');
      MyButton.setProperty('background-color', 'blueviolet');
      MyButton.OnClick := @doMyButtonOnClick;
    end;

    procedure doJInput1;
    var
      Input1: TWInput;
      MySelect: TWSelect;
      Types: Array of String;
      i: Integer;
      Item1: TWPanel;

      procedure doItem1OnClick;
      begin
        Input1.SetAttribute('type',MySelect.Value);

        case Myselect.Value of
          'button'   : Input1.SetAttribute('value','input type = ' + MySelect.Value);
          'color'    : Input1.SetAttribute('value','#ff0000');
          'email'    : Input1.SetAttribute('placeholder','user@host.com');
          'file'     : Input1.SetAttribute('accept','.jpg, .jpeg, .png');
          'hidden'   : Input1.SetAttribute('value','should not see this');
          'image'    : Input1.SetAttribute('src','images/logo.png');
          'number'   : Input1.SetAttribute('value','123456');
          'password' : Input1.SetAttribute('required','true');
          'radio'    : Input1.SetAttribute('checked','true');
          'reset'    : Input1.SetAttribute('value','reset form');
          'search'   : Input1.SetAttribute('placeholder','search...');
          'submit'   : Input1.SetAttribute('value','submit form');
          'tel'      : Input1.SetAttribute('placeholder','+61(0)x 9999 9999');
          'text'     : Input1.SetAttribute('value','input type = ' + MySelect.Value);
          'url'      : Input1.SetAttribute('placeholder','http://www.lynkfs.com');
        end;
        TJSHTMLElement(MySelect.Handle).style.setProperty('display', 'none');
      end;

    begin
      {--------------------------------------------------------------------------}
      { Component  TWInput (combobox)                                           }
      { Updated:   01/09/2017                                                    }
      {--------------------------------------------------------------------------}
      Input1 := TWInput.Create(DisplayDiv);
      Input1.SetBounds(0, 0, 200, 40);
      Input1.SetProperty('border','2px solid whitesmoke');
  //
        MySelect := TWSelect.Create(self);
        //save this id on the form
        self.Handle.setAttribute('data-select', MySelect.Handle.id);

        MySelect.SetBounds(17, 290, 200, 200);
        MySelect.setProperty('background-color', 'white');

        TJSArray(Types).Length:= 0; // ==> Types.Clear;
        TJSArray(Types).Push('button');
        TJSArray(Types).Push('checkbox');
        TJSArray(Types).Push('color');
        TJSArray(Types).Push('date');
        TJSArray(Types).Push('datetime-local');
        TJSArray(Types).Push('email');
        TJSArray(Types).Push('file');
        TJSArray(Types).Push('hidden');
        TJSArray(Types).Push('image');
        TJSArray(Types).Push('month');
        TJSArray(Types).Push('number');
        TJSArray(Types).Push('password');
        TJSArray(Types).Push('radio');
        TJSArray(Types).Push('range');
        TJSArray(Types).Push('reset');
        TJSArray(Types).Push('search');
        TJSArray(Types).Push('submit');
        TJSArray(Types).Push('tel');
        TJSArray(Types).Push('text');
        TJSArray(Types).Push('time');
        TJSArray(Types).Push('url');
        TJSArray(Types).Push('week');

        For i := 0 to TJSArray(Types).Length - 1 do
        begin
          Item1 := TWPanel.Create(MySelect);
          Item1.setProperty('background-color', 'whitesmoke');
          Item1.Height := 20;
          Item1.SetinnerHTML(Types[i]);
          Item1.tag := Types[i];
          Item1.Handle.addEventListener('click', @doItem1OnClick);
          MySelect.Add(Item1);
        end;
    end;


Begin
  inherited;
  //ToolBar.SetActiveMenu('Form1');
  //Init Logo
  Image0 := TWImage.Create(self);
  Image0.SetBounds(0, 0, 194, 45);
  Image0.setAttribute('src','images/logo.png');

  //Init ToolBar
  ToolBar := TWToolBar.Create(self);
  ToolBar.SetBounds(0, 45, 0, 40);
  ToolBar.setProperty('min-width','100%');
  ToolBar.setProperty('background-color', '#699BCE');
  ToolBar.AddMenu('Components','Form1','white');
  ToolBar.AddMenu('Projects','Form2','white');
  ToolBar.SetActiveMenu('Form1');

(*  Fpanel1 := TWPanel.Create(Self);
  Fpanel1.setProperty('background-color', 'silver');
  Fpanel1.SetBounds(150, 10, 100, 35);
  Fpanel1.tag := IntToStr(i);
  Fpanel1.OnClick := @doPanel0OnClick;

  Fbutton1 := TWButton.Create(Self);
  Fbutton1.SetBounds(150, 50, 100, 50);
  Fbutton1.SetInnerHTML('Button1');
  Fbutton1.OnClick := @doPanel1OnClick;*)

  //create kitchen sink components and add to Component array
  TJSArray(Components).Length:=0;  //--> Components.clear;
  DisplayDiv := TWPanel.Create(self);
  DisplayDiv.SetBounds(20, 300, 0, 0);

  (*===== components ======*)
  //JPanel
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JPanel';
  ComponentRec.ShowIt := @doPanel1;
  TJSArray(Components).Push(ComponentRec);

  //JButton
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JButton';
  ComponentRec.ShowIt := @doButton1;
  TJSArray(Components).Push(ComponentRec);

  //JProgress
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JProgress';
  ComponentRec.ShowIt := @doProgress1;
  TJSArray(Components).Push(ComponentRec);

  //JImage
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JImage';
  ComponentRec.ShowIt := @doImage1;
  TJSArray(Components).Push(ComponentRec);

  //JSplitter
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JSplitter';
  ComponentRec.ShowIt := @doSplitter1;
  TJSArray(Components).Push(ComponentRec);

  //JToolBar
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JToolBar';
  ComponentRec.ShowIt := @doJToolBar1;
  TJSArray(Components).Push(ComponentRec);

  //JSelect
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JSelect';
  ComponentRec.ShowIt := @doJSelect1;
  TJSArray(Components).Push(ComponentRec);

  //JListBox
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JListBox';
  ComponentRec.ShowIt := @doJListBox1;
  TJSArray(Components).Push(ComponentRec);

  //JVideo
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JVideo';
  ComponentRec.ShowIt := @doJVideo1;
  TJSArray(Components).Push(ComponentRec);

  //JAnchor
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JAnchor';
  ComponentRec.ShowIt := @doJAnchor1;
  TJSArray(Components).Push(ComponentRec);

  //JTextArea
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JTextArea';
  ComponentRec.ShowIt := @doJTextArea1;
  TJSArray(Components).Push(ComponentRec);

  //JIFrame
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'TWebPage (IFrame)';
  ComponentRec.ShowIt := @doJIFrame1;
  TJSArray(Components).Push(ComponentRec);

  //JFlipPage
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JFlipScroll - under constr';
  ComponentRec.ShowIt := @doJFlipPage1;
  TJSArray(Components).Push(ComponentRec);

  //JLoader
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JLoader';
  ComponentRec.ShowIt := @doJLoader1;
  TJSArray(Components).Push(ComponentRec);

  //JSpinner
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JSpinner';
  ComponentRec.ShowIt := @doJSpinner1;
  TJSArray(Components).Push(ComponentRec);

  //JGrid
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JGrid';
  ComponentRec.ShowIt := @doJGrid1;
  TJSArray(Components).Push(ComponentRec);

  //JCanvas
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JCanvas';
  ComponentRec.ShowIt := @doJCanvas1;
  TJSArray(Components).Push(ComponentRec);

  //JTreeView
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JTreeView';
  ComponentRec.ShowIt := @doJTreeView1;
  TJSArray(Components).Push(ComponentRec);

  //JCheckBox single
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JCheckBox (single)';
  ComponentRec.ShowIt := @doJCheckBox1;
  TJSArray(Components).Push(ComponentRec);

  //JCheckBox multiple
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JCheckBox (multiple)';
  ComponentRec.ShowIt := @doJCheckBox2;
  TJSArray(Components).Push(ComponentRec);

  //JRadioButton
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JRadioButtons';
  ComponentRec.ShowIt := @doJRadioButton1;
  TJSArray(Components).Push(ComponentRec);

  //TWindow
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'TWindow - under constr.';
  ComponentRec.ShowIt := @doTWindow1;
  TJSArray(Components).Push(ComponentRec);

  //JDialog
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JDialog - under constr.';
  ComponentRec.ShowIt := @doJDialog1;
  TJSArray(Components).Push(ComponentRec);

  //JInput
  ComponentRec := TComponentRec.Create;
  ComponentRec.Name := 'JInput';
  ComponentRec.ShowIt := @doJInput1;
  TJSArray(Components).Push(ComponentRec);


(*
//JListBox
  ListBox1 := TWListBox.Create(self);
  ListBox1.SetBounds(20, 150, trunc(screenwidth*0.8), 200);
  ListBox1.setProperty('background-color', 'gold');

  For i := 1 to 50 do begin
    Panel0 := TWPanel.Create(ListBox1);
    Panel0.setProperty('background-color', 'silver');
    Panel0.height := 30;
    Panel0.tag := inttostr(i);
    Panel0.OnClick := @doPanel0OnClick;

    Panel1 := TWPanel.Create(Panel0);
    Panel1.setProperty('background-color', 'lightgreen');
    Panel1.SetBounds(30,2,26,26);
    Panel1.SetinnerHTML(IntToStr(i));
    Panel1.tag := inttostr(i);
    Panel1.OnClick := @doPanel1OnClick;

    ListBox1.Add(Panel0);
  end;*)

  populateListBox;

end;

procedure TForm1.Resize;
begin
  inherited;
end;


end.

