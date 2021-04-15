<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright 2002 Geodise Project, University of Southampton -->
<!-- matlabName2Type.xsl - Jasmin Wason - 27/11/02 -->
<!-- XSLT stylesheet to transform XML name-based documents back into their original format. -->
<!-- The input documents must have been originally created using matlabType2Name.xsl -->
<!-- The output documents will be parsed by the xml_parse function written by Marc Molinari -->
<!-- i.e. The output documents must conform to the schema http://www.geodise.org/matlab.xsd -->
<!-- The documents are tranformed from a name-based format into a type-based format -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.geodise.org/matlab.xsd">
	<xsl:output omit-xml-declaration="no" method="xml" indent="yes"/>

	<!-- The original root element will not have a name attribute, so 'metadata' is used as the new root name -->
	<!-- The original element name of root is made the value of the 'type' attribute in the new document-->
	<!-- If size and fields exist they are added to the new document without being changed -->
	<xsl:template match="/">
		<xsl:variable name="oldName">
				<xsl:value-of select="*/@type"/>
		</xsl:variable>
		<xsl:element name="{$oldName}" namespace="http://www.geodise.org/matlab.xsd">
				<!--<xsl:attribute name="name"><xsl:value-of select="name(*)"/></xsl:attribute>-->
				<xsl:attribute name="idx"><xsl:value-of select="*/@idx"/></xsl:attribute>
				<xsl:if test="*/@size">
					<xsl:attribute name="size"><xsl:value-of select="*/@size"/></xsl:attribute>
				</xsl:if>
				<xsl:if test="*/@fields">
					<xsl:attribute name="fields"><xsl:value-of select="*/@fields"/></xsl:attribute>
				</xsl:if>
				<!-- Tranform child elements -->
				<xsl:choose>
					<!-- If empty text, prevent newline being inserted -->
					<xsl:when test=".=''">
						<xsl:text></xsl:text>
					</xsl:when>
					<xsl:otherwise>
						<xsl:for-each select="*">
							<xsl:apply-templates/>
						</xsl:for-each>
					</xsl:otherwise>
				</xsl:choose> 
		</xsl:element>
	</xsl:template>

	<xsl:template match="*">
		<!-- Set variable newName to be the new name for this element -->
		<!-- which is either the value of the name attribute, or 'item' if there is no name attribute-->
		<xsl:variable name="oldName">
				<xsl:value-of select="@type"/>
		</xsl:variable>
		<!-- Create an element called $newName -->
		<!-- The original element name is made the value of the 'type' attribute in the new document-->
		<!-- If size and fields exist they are added to the new document without being changed -->
		<xsl:element name="{$oldName}">
			<xsl:choose>
				<xsl:when test="name()='item'"></xsl:when>
				<xsl:otherwise><xsl:attribute name="name"><xsl:value-of select="name()"/></xsl:attribute>
				</xsl:otherwise>
			</xsl:choose> 
			
			<xsl:attribute name="idx"><xsl:value-of select="@idx"/></xsl:attribute>
			<xsl:if test="@size">
				<xsl:attribute name="size"><xsl:value-of select="@size"/></xsl:attribute>
			</xsl:if>		
			<xsl:if test="@fields">
				<xsl:attribute name="fields"><xsl:value-of select="@fields"/></xsl:attribute>
			</xsl:if>
			<!-- Tranform child elements -->
			<xsl:choose>
				<!-- If empty text, prevent newline being inserted -->
				<xsl:when test=".=''">
					<xsl:text></xsl:text>
				</xsl:when>
				<xsl:otherwise>
					<xsl:apply-templates/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:element>
	</xsl:template>

</xsl:stylesheet> 

