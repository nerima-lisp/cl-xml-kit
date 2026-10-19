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
\" \"<!-- trailing -->\")"))
