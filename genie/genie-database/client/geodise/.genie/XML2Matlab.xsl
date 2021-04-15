<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright 2003 Geodise Project, University of Southampton -->
<!-- XML2Matlab.xsl - Jasmin Wason - 23/06/03 -->
<!-- XSLT stylesheet to transform XML documents into the Matlab XML format that is recognised by the XML Toolbox 1.0 Geodise Database Toolkit 0.5 -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output omit-xml-declaration="no" method="html" indent="yes"/>
	<!-- new root element name can be passed in to the stylesheet. Default is file_metadata -->
	<xsl:param name="metadataType">file_metadata</xsl:param>
	<!-- name of custom metadata root can be passed in to the stylesheet for removal. Default is metadata -->
	<xsl:param name="customRootName">metadata</xsl:param>
	<!-- Match the root element -->
	<xsl:template match="/">
		<!-- The root element is given the name that was passed in as a parameter, or file_metadata if no parameter was passed -->
		<xsl:element name="{$metadataType}">
			<!-- Add Matlab attributes for the root element (idx is always 0 for the root) -->
			<xsl:call-template name="addAttributes">
				<xsl:with-param name="idx">0</xsl:with-param>
				<xsl:with-param name="type">struct</xsl:with-param>
			</xsl:call-template>
			<!-- If there is no standard element add one -->
			<!--			<xsl:if test="not(*/standard)"> -->
			<!-- Add Matlab attributes to standard -->
			<!--
				<xsl:element name="standard">
					<xsl:call-template name="addAttributes">
						<xsl:with-param name="type">struct</xsl:with-param>						
					</xsl:call-template>
				</xsl:element>
			</xsl:if>
-->
			<xsl:choose>
				<!-- If empty text, prevent newline being inserted -->
				<xsl:when test=".=''">
					<xsl:text/>
				</xsl:when>
				<!-- Otherwise process all child elements -->
				<xsl:otherwise>
					<xsl:for-each select="*">
						<xsl:apply-templates/>
					</xsl:for-each>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:element>
	</xsl:template>
	<!--Match children of file or varmeta or datagroup (standard metadata)-->
	<!--This is done so that fixed standard metadata is always given the right type-->
	<!--E.g. even a localName consisting of numbers is still classed as a char -->
	<xsl:template match="file/*|varmeta/*|datagroup/*|vardata/*">
		<!-- Create an element with the same name -->
		<xsl:element name="{name()}">
			<xsl:choose>
				<!-- if this element has children make it a struct and process them -->
				<xsl:when test="*">
					<!-- First add Matlab attributes to this element -->
					<xsl:call-template name="addAttributes">
						<xsl:with-param name="type">struct</xsl:with-param>
					</xsl:call-template>
					<xsl:apply-templates/>
				</xsl:when>
				<!-- if this element does not have children make it a char or double -->
				<xsl:otherwise>
					<xsl:choose>
						<!-- Add Matlab attributes-->
						<xsl:when test=".=''">
							<!--  If element is empty set size to 0 0 and make it a char-->
							<xsl:call-template name="addAttributes">
								<xsl:with-param name="type">char</xsl:with-param>
								<xsl:with-param name="size">0 0</xsl:with-param>
							</xsl:call-template>
							<!-- Empty text -->
							<xsl:text/>
						</xsl:when>
						<xsl:otherwise>
							<!-- Set the datatype -->
							<xsl:variable name="datatype">
								<xsl:choose>
									<xsl:when test="name()='ID'">char</xsl:when>
									<xsl:when test="name()='localName'">char</xsl:when>
									<xsl:when test="name()='byteSize'">double</xsl:when>
									<xsl:when test="name()='format'">char</xsl:when>
									<xsl:when test="name()='createDate'">char</xsl:when>
									<xsl:when test="name()='archiveDate'">char</xsl:when>
									<xsl:when test="name()='userID'">char</xsl:when>
									<xsl:when test="name()='comment'">char</xsl:when>
									<xsl:when test="name()='version'">double</xsl:when>
									<!-- If the element does not have one of these standard names then guess the datatype-->
									<xsl:otherwise>
										<xsl:choose>
											<xsl:when test="number()">double</xsl:when>
											<xsl:otherwise>char</xsl:otherwise>
										</xsl:choose>
									</xsl:otherwise>
								</xsl:choose>
							</xsl:variable>
							<xsl:call-template name="addAttributes">
								<xsl:with-param name="type">
									<xsl:value-of select="$datatype"/>
								</xsl:with-param>
								<xsl:with-param name="size">
									<xsl:choose>
										<xsl:when test="$datatype='double'">1 1</xsl:when>
										<xsl:when test="$datatype='char'">1 <xsl:value-of select="string-length(.)"/>
										</xsl:when>
									</xsl:choose>
								</xsl:with-param>
							</xsl:call-template>
							<!-- Set the value of this element -->
							<xsl:value-of select="."/>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:element>
	</xsl:template>
	<!-- Match any child element -->
	<xsl:template match="*">
		<!-- Create an element with the same name then process its children -->
		<xsl:element name="{name()}">
			<xsl:choose>
				<xsl:when test="*">
					<!-- First add Matlab attributes to this element -->
					<xsl:call-template name="addAttributes">
						<xsl:with-param name="type">struct</xsl:with-param>
					</xsl:call-template>
					<xsl:apply-templates/>
				</xsl:when>
				<!-- if this element does not have children make it a char or double -->
				<xsl:otherwise>
					<xsl:choose>
						<!-- Add Matlab attributes-->
						<xsl:when test=".=''">
							<!--  If element is empty set size to 0 0 and make it a char-->
							<xsl:call-template name="addAttributes">
								<xsl:with-param name="type">char</xsl:with-param>
								<xsl:with-param name="size">0 0</xsl:with-param>
							</xsl:call-template>
							<!-- Empty text -->
							<xsl:text/>
						</xsl:when>
						<xsl:otherwise>
							<!-- otherwise check if this is a number -->
							<xsl:choose>
								<xsl:when test="number()">
									<!-- if number set type to double and size to 1 1 -->
									<xsl:call-template name="addAttributes">
										<xsl:with-param name="type">double</xsl:with-param>
										<xsl:with-param name="size">1 1</xsl:with-param>
									</xsl:call-template>
								</xsl:when>
								<xsl:otherwise>
									<!-- otherwise set type to char and size to 1 and the size of the string -->
									<xsl:call-template name="addAttributes">
										<xsl:with-param name="type">char</xsl:with-param>
										<xsl:with-param name="size">1 <xsl:value-of select="string-length(.)"/>
										</xsl:with-param>
									</xsl:call-template>
								</xsl:otherwise>
							</xsl:choose>
							<!-- Set the value of this element -->
							<xsl:value-of select="."/>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:element>
	</xsl:template>
	<!-- Template to add Matlab specific attributes (type, idx and size) and any other attributes already present -->
	<xsl:template name="addAttributes">
		<xsl:param name="type">struct</xsl:param>
		<xsl:param name="idx">1</xsl:param>
		<xsl:param name="size">1 1</xsl:param>
		<xsl:for-each select="@*">
			<xsl:attribute name="{name()}"><xsl:value-of select="."/></xsl:attribute>
		</xsl:for-each>
		<xsl:if test="not(@type)">
			<xsl:attribute name="type"><xsl:value-of select="$type"/></xsl:attribute>
		</xsl:if>
		<xsl:if test="not(@idx)">
			<xsl:attribute name="idx"><xsl:value-of select="$idx"/></xsl:attribute>
		</xsl:if>
		<xsl:if test="not(@size)">
			<xsl:attribute name="size"><xsl:value-of select="$size"/></xsl:attribute>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>
