<map version="0.7.1">
<node TEXT="Generalized Wiki">
<cloud/>
<font NAME="SansSerif" BOLD="true" SIZE="16"/>
<node TEXT="Why GW?" POSITION="left">
<font NAME="SansSerif" BOLD="true" SIZE="14"/>
<node TEXT="Vision"/>
<node TEXT="An improved and more flexible Wiki">
<node TEXT="Shortcomings of existing Wikis">
<node TEXT="Storage management">
<node TEXT="No hierarchical structure">
<node TEXT="Difficult to manage projects"/>
<node TEXT="One namespace"/>
<node TEXT="TWiki has one level structure with Webs"/>
</node>
<node TEXT="Non-wiki files managed via different &apos;attachment&apos; mechanism"/>
<node TEXT="File based version management"/>
</node>
<node TEXT="Limited user management">
<node TEXT="Caching interferes with authentication"/>
</node>
<node TEXT="Markup">
<node TEXT="Unstructured markup"/>
<node TEXT="Only wiki markup"/>
</node>
<node TEXT="Configuration not managable via wiki editing"/>
<node TEXT="Templates">
<node TEXT="Uses a &apos;propietary&apos; and ad-hoc template mechanism"/>
<node TEXT="Templates are not configurable by wiki users"/>
</node>
<node TEXT="Monolithic design">
<node TEXT="Difficult to add alternative renderings"/>
<node TEXT="Extend in other ways"/>
<node TEXT="Adapt/reuse only part of the implementation"/>
</node>
</node>
<node TEXT="Improvements in GW" FOLDED="true">
<node TEXT="Site wide version management with subversion"/>
<node TEXT="Fine grained access control for writing *and* reading"/>
<node TEXT="Templates can be adapted just like other aspects of the wiki"/>
<node TEXT="Plugin redering schemes allows support for new markup formats and other file formats"/>
<node TEXT="Forms allow storing and editing of structured documents">
<font NAME="SansSerif" SIZE="12"/>
</node>
</node>
<node TEXT="Compare to database systems" FOLDED="true">
<node TEXT="Databases don&apos;t support versioning and files"/>
</node>
</node>
<node TEXT="New applications">
<node TEXT="Conference submission and reviewing system" FOLDED="true">
<node TEXT="role-based access control"/>
<node TEXT="Access control for composite results"/>
</node>
<node TEXT="Issue tracking"/>
<node TEXT="Bibtex server"/>
</node>
</node>
<node TEXT="Use Cases" POSITION="left">
<font NAME="SansSerif" BOLD="true" SIZE="14"/>
<node TEXT="View" FOLDED="true">
<node TEXT="Show raw text"/>
<node TEXT="Content is HTML"/>
<node TEXT="Render wiki markup"/>
<node TEXT="Mime Type"/>
</node>
<node TEXT="Edit" FOLDED="true">
<node TEXT="Edit file contents"/>
<node TEXT="Preview"/>
<node TEXT="Save"/>
</node>
<node TEXT="RecentChanges" FOLDED="true">
<node TEXT="A list of the X most recently changed pages"/>
</node>
<node TEXT="Search" FOLDED="true">
<node TEXT="Find pages that match a query on the textual content"/>
</node>
</node>
<node TEXT="Components/Projects" FOLDED="true" POSITION="right">
<font NAME="SansSerif" BOLD="true" SIZE="14"/>
<node TEXT="Storage" FOLDED="true">
<node TEXT="I/O"/>
</node>
<node TEXT="User management" FOLDED="true">
<node TEXT="Cryptography"/>
<node TEXT=""/>
</node>
<node TEXT="Rendering" FOLDED="true">
<node TEXT="Wiki Rendering"/>
</node>
<node TEXT="Templates" FOLDED="true">
<node TEXT="XSLT"/>
</node>
<node TEXT="Versioning UI">
<node TEXT="Manipulate versioned files and directories"/>
<node TEXT="Information retrieval" FOLDED="true">
<node TEXT="Head revision of file"/>
<node TEXT="Most recently changed files in a directory"/>
</node>
<node TEXT="Modifications" FOLDED="true">
<node TEXT="Commit new version"/>
<node TEXT="Revert to old version"/>
<node TEXT="Merge with branch"/>
</node>
</node>
<node TEXT="Visualization" FOLDED="true">
<node TEXT="Graphviz"/>
<node TEXT="RSS feeds"/>
</node>
<node TEXT="Forms" FOLDED="true">
<node TEXT="XML"/>
</node>
<node TEXT="Query/Search" FOLDED="true">
<node TEXT="Database management"/>
<node TEXT="Text search"/>
</node>
</node>
<node TEXT="Tasks" FOLDED="true" POSITION="right">
<font NAME="SansSerif" BOLD="true" SIZE="14"/>
<node TEXT="User documentation">
<node TEXT="This should be developed on the new wiki"/>
<node TEXT="Test case for the wiki"/>
</node>
<node TEXT="Technical documentation">
<node TEXT="Should enable new developers to understand the organization of the project"/>
<node TEXT="JavaDoc for Java programs"/>
<node TEXT="Other documentation for components in other languages"/>
</node>
<node TEXT="Integration">
<node TEXT="Integrate components"/>
<node TEXT="Test the integration"/>
</node>
<node TEXT="Testing">
<node TEXT="Develop unit and regression tests"/>
</node>
</node>
</node>
</map>
