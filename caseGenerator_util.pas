unit caseGenerator_util;

interface

function ExtractField(line, subLine: String): String;

implementation
uses
  System.Classes;

{ utlCase }

function ExtractField(line, subLine: String): String;
var
  fieldValue: String;
  fieldStart, fieldEnd: Integer;
begin
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  fieldValue := Copy(line, fieldStart + Length(subLine), Length(line));
  fieldEnd   := Pos('"', fieldValue);
  Result := Copy(fieldValue, 1, fieldEnd - 1);

end;

function ExtractArray(line, subLine: String): TStringList;
var
  fieldValue: String;
  fieldStart, fieldEnd: Integer;
  itemsArray: String;
  commaPos: Integer;
  itemsList: TStringList;
begin
  fieldStart := Pos(subLine, line); // search pos of subLine inside line
  fieldValue := Copy(line, fieldStart + Length(subLine), Length(line));
  fieldEnd   := Pos(']', fieldValue);
  itemsArray := Copy(fieldValue, 1, fieldEnd - 1);
  itemsList := TStringList.Create;
  commaPos := Pos(',', itemsArray);
  while commaPos >= 0 do
  begin
    itemsList.Add(Copy(itemsArray, 1, commaPos - 1));
    itemsArray := Copy(itemsArray, commaPos + 1, Length(itemsArray));
    commaPos := Pos(',', itemsArray);
  end;
  itemsList.Add(itemsArray);
  Result := itemsArray;


end;

end.

