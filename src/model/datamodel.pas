unit datamodel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, XMLHelper, fgl;

type
  { TProject }

 TProject = class
    public
      ProjectName: UnicodeString;
      Comment: UnicodeString;
      SelectedLanguage: Integer;
      Doc: TXMLDocument;
      constructor Create;
      destructor Destroy; override;
      procedure Save(FileName: String);
      procedure Load(FileName: String);
      procedure AddTextNode(ParentNode: TDOMNode; const NodeName, NodeValue: String);
      function GetTextNodeContent(Node: TDOMNode): String;
  end;


implementation


{ TProject }

constructor TProject.Create;
begin
  inherited Create;
end;

destructor TProject.Destroy;
begin
  inherited Destroy;
end;

procedure TProject.AddTextNode(ParentNode: TDOMNode; const NodeName, NodeValue: String);
begin
  ParentNode.AppendChild(Doc.CreateElement(NodeName)).TextContent := NodeValue;
end;



procedure TProject.Save(FileName: String);
var
  RootNode: TDOMNode;
  i, j: Integer;
begin
  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('Project');
    Doc.AppendChild(RootNode);

    AddTextNode(RootNode, 'FileVersion', '1.0');
    AddTextNode(RootNode, 'ProjectName', ProjectName);
    AddTextNode(RootNode, 'Comment', Comment);
    AddTextNode(RootNode, 'SelectedLanguage', IntToStr(SelectedLanguage));

    WriteXMLFile(Doc, FileName);
  finally
    Doc.Free;
  end;
end;

procedure TProject.Load(FileName: String);
var
  RootNode:  TDOMNode;
begin
  ReadXMLFile(Doc, FileName);
  try
    RootNode := Doc.DocumentElement;

    ProjectName := TXMLHelper.GetXML('ProjectName', RootNode);
    Comment:= TXMLHelper.GetXml('Comment', RootNode);

  finally
    Doc.Free;
  end;
end;


function TProject.GetTextNodeContent(Node: TDOMNode): String;
  begin
    if Assigned(Node) and Assigned(Node.FirstChild) then
      Result := Node.FirstChild.NodeValue
    else
      Result := '';
  end;


end.

