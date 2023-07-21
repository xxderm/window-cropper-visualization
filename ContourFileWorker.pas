unit ContourFileWorker;

interface

uses
  System.Types, System.SysUtils, System.Variants, System.Classes, Contour;

type
  TContourLoader = class(TInterfacedObject)
  public
    procedure processFile(const fileName: string; var contours: TContours);
    procedure saveContour(const fileName: string; const contour: IContour);
  end;

var
  Loader: TContourLoader;

implementation

{ TContourLoader }

procedure TContourLoader.saveContour(const fileName: string; const contour: IContour);
var
  i: Integer;
  outFile: TextFile;
begin
  AssignFile(outFile, fileName);
  try
    Rewrite(outFile);
    for i := 0 to contour.getContourBitCount - 1 do
    begin
      WriteLn(outFile,
        FloatToStr(contour.getContourBit(i).getPoint(0).getX) + ' ' +
        FloatToStr(contour.getContourBit(i).getPoint(0).getY));
    end;
  finally
    CloseFile(outFile);
  end;
end;

procedure TContourLoader.processFile(const fileName: string; var contours: TContours);
var
  Lines: TStringList;
  X, Y: Integer;
  contourBit: TContourBitEdit2;
  contour: TContourEdit2;
  i: Integer;
begin
  Lines := TStringList.Create;

  contour := TContourEdit2.Create;
  try
    Lines.LoadFromFile(fileName);

    for i := 1 to Lines.Count - 1 do
    begin
      contourBit := TContourBitEdit2.Create;

      X := StrToIntDef(Copy(Lines[i], 1, Pos(' ', Lines[i]) - 1), 0);
      Y := StrToIntDef(Copy(Lines[i], Pos(' ', Lines[i]) + 1, Length(Lines[i])), 0);

      // Добавляем точку во фрагмент контура
      contourBit.addPoint(X, Y, 0);

      if (Lines.Count - 1) mod 2 = 0 then
        contourBit.setClosed(False) // Незамкнутый контурбит
      else contourBit.setClosed(True); // Замкнутый контурбит

      // Добавляем фрагмент контура в контур
      contour.addContourBit(ContourBit);
    end;

  finally
     Lines.Free;

     // Добавляем контур в коллекцию
      contours.addContour(contour);
  end;

end;

end.
