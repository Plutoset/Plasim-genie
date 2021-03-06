<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright 2002 Geodise Project, University of Southampton -->
<!-- matlabType2Name.xsl - Jasmin Wason - 27/11/02 -->
<!-- XSLT stylesheet to transform XML documents generated by the xml_save function written by Marc Molinari -->
<!-- i.e. XML documents conforming to the schema http://www.geodise.org/matlab.xsd -->
<!-- The documents are tranformed from a type-based format into a name-based format -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:m="http://www.geodise.org/matlab.xsd">
	<xsl:output omit-xml-declaration="no" method="xml" indent="yes"/>
	<xsl:strip-space elements="*"/>
	<!-- new root element name can be passed in to the stylesheet. Default is file_metadata -->
	<xsl:param name="metadataType">file_metadata</xsl:param>
	<!-- The original root element will not have a name attribute, so 'metadata' is used as the new root name -->
	<!-- The original element name of root is made the value of the 'type' attribute in the new document-->
	<!-- If size and fields exist they are added to the new document without being changed -->
	<xsl:template match="/">
		<xsl:apply-templates/>
	</xsl:template>
	<xsl:template match="*">
		<xsl:element name="{name()}">
			<!-- Tranform child elements -->



					<xsl:choose>
						<xsl:when test="@type='char' and text()">
							<xsl:variable name="test">
								<xsl:choose>
									<xsl:when test="string-length(normalize-space(text()))>0">
										<xsl:call-template name="remove-leading-whitespace">
											<xsl:with-param name="str" select="text()"/>
										</xsl:call-template>
									</xsl:when>
									<xsl:otherwise>
										<xsl:value-of select="text()"/>
									</xsl:otherwise>
								</xsl:choose>
							</xsl:variable>
							<xsl:call-template name="add-attributes">
								<xsl:with-param name="size">1 <xsl:value-of select="string-length($test)"/>
								</xsl:with-param>
							</xsl:call-template>
							<xsl:value-of select="$test"/>
						</xsl:when>
						<xsl:otherwise>
							<xsl:call-template name="add-attributes"/>
							<xsl:apply-templates/>
						</xsl:otherwise>
					</xsl:choose>

		</xsl:element>
	</xsl:template>
	<xsl:template name="add-attributes">
		<xsl:param name="size"/>
		<xsl:for-each select="@*">
			<xsl:choose>
				<xsl:when test="name()='size' and $size">
					<xsl:attribute name="size"><xsl:value-of select="$size"/></xsl:attribute>
				</xsl:when>
				<xsl:otherwise>
					<xsl:attribute name="{name()}"><xsl:value-of select="."/></xsl:attribute>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>
	</xsl:template>
	<xsl:template name="remove-leading-whitespace">
		<xsl:param name="str"/>
		<xsl:choose>
			<xsl:when test="starts-with($str,' ')">
				<xsl:call-template name="remove-leading-whitespace">
					<xsl:with-param name="str">
						<xsl:value-of select="substring($str,2)"/>
					</xsl:with-param>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$str"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
