(("spans/fixture-slices" . "(\"<!DOCTYPE root [
  <!ENTITY e \\\"text\\\">
  <!ENTITY m \\\"<b/>\\\">
  <!ATTLIST item id ID #IMPLIED>
]>\" \"<!-- top comment -->\" \"<?pi data?>\" \"<root xmlns=\\\"urn:default\\\" xmlns:p=\\\"urn:p\\\"
      a='single'
      b=\\\"double\\\">
  <item id=\\\"1\\\">text &amp; &#65; &e; <![CDATA[<raw>]]></item>
  <p:item xmlns:p=\\\"urn:other\\\"/>
  <!-- inner
       comment -->
  <?inner pi?>
  <empty />
</root>\" \"<root xmlns=\\\"urn:default\\\" xmlns:p=\\\"urn:p\\\"
      a='single'
      b=\\\"double\\\">\" \"
  <item id=\\\"1\\\">text &amp; &#65; &e; <![CDATA[<raw>]]></item>
  <p:item xmlns:p=\\\"urn:other\\\"/>
  <!-- inner
       comment -->
  <?inner pi?>
  <empty />
\" \" xmlns=\\\"urn:default\\\"\" \" xmlns:p=\\\"urn:p\\\"\" \"
      a='single'\" \"
      b=\\\"double\\\"\" \"
  \" \"<item id=\\\"1\\\">text &amp; &#65; &e; <![CDATA[<raw>]]></item>\" \"<item id=\\\"1\\\">\" \"text &amp; &#65; &e; <![CDATA[<raw>]]>\" \" id=\\\"1\\\"\" \"text &amp; &#65; &e; <![CDATA[<raw>]]>\" \"
  \" \"<p:item xmlns:p=\\\"urn:other\\\"/>\" \"<p:item xmlns:p=\\\"urn:other\\\"/>\" \"\" \" xmlns:p=\\\"urn:other\\\"\" \"
  \" \"<!-- inner
       comment -->\" \"
  \" \"<?inner pi?>\" \"
  \" \"<empty />\" \"<empty />\" \"\" \"
\" \"<!-- trailing -->\")")
 ("parser/mixed-content[0]" . ":start-document")
 ("parser/mixed-content[1]" . ":comment")
 ("parser/mixed-content[2]" . ":processing-instruction")
 ("parser/mixed-content[3]" . ":doctype")
 ("parser/mixed-content[4]" . ":start-element")
 ("parser/mixed-content[5]" . ":characters")
 ("parser/mixed-content[6]" . ":characters")
 ("parser/mixed-content[7]" . ":start-element")
 ("parser/mixed-content[8]" . ":end-element")
 ("parser/mixed-content[9]" . ":characters")
 ("parser/mixed-content[10]" . ":end-element")
 ("parser/mixed-content[11]" . ":comment")
 ("parser/mixed-content[12]" . ":end-document"))
