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
\" \"<!-- trailing -->\")") ("serializer/nested-namespaces" . "\"<root xmlns=\\\"urn:default\\\" xmlns:p=\\\"urn:p1\\\" xmlns:a=\\\"urn:a\\\" a:id=\\\"root\\\"><p:child xmlns:p=\\\"urn:p2\\\" p:code=\\\"one\\\"><plain xmlns=\\\"\\\" a:kind=\\\"plain\\\"><p:leaf xmlns:p=\\\"urn:p3\\\"/></plain></p:child></root>\""))
