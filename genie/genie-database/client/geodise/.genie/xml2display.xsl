<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>
      <xsl:strip-space elements = "*"/>
	<xsl:param name="ishtml">
		<xsl:value-of select="true()"/>
	</xsl:param>
	<xsl:template match="/">
	
		<xsl:for-each select="*">
			<xsl:apply-templates/>
		</xsl:for-each>
	</xsl:template>
	<xsl:template name="temp" match="*">
		<xsl:param name="parent">
			<xsl:value-of select="name()"/>
		</xsl:param>
		<!-- Change ishtml to true() for html output and false() for text output -->
		<!--
		<xsl:variable name="ishtml">
			<xsl:value-of select="true()"/>
		</xsl:variable>
		-->
		<xsl:choose>
			<xsl:when test="text()">
				<xsl:value-of select="$parent"/> = <xsl:value-of select="."/>
				<xsl:choose>
					<xsl:when test="$ishtml='true'">
						<br/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:text>&#xA;</xsl:text>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:otherwise>
				<xsl:choose>
					<xsl:when test="@type='cell'">
						<xsl:value-of select="$parent"/> = {<xsl:value-of select="translate(@size, ' ', 'x')"/>
						<xsl:text> </xsl:text>
						<xsl:value-of select="@type"/>
						<xsl:text>}</xsl:text>
					</xsl:when>
					<xsl:when test="@type='complex'">
						<xsl:value-of select="$parent"/>
						<xsl:text> = </xsl:text>
						<xsl:choose>
							<xsl:when test="@size='1 1' or not(@size)">
								<xsl:value-of select="*[1]"/>+<xsl:value-of select="*[2]"/>
								<xsl:text>i</xsl:text>
							</xsl:when>
							<xsl:otherwise>
								<xsl:text>[</xsl:text>
								<xsl:value-of select="translate(@size, ' ', 'x')"/>
								<xsl:text> </xsl:text>
								<xsl:value-of select="*[1]/@type"/>
								<xsl:text>]</xsl:text>
							</xsl:otherwise>
						</xsl:choose>
					</xsl:when>
					<xsl:when test="@type='sparse' ">
						<xsl:value-of select="$parent"/> = [<xsl:value-of select="translate(@size, ' ', 'x')"/>
						<xsl:text> </xsl:text>
						<xsl:value-of select="*[1]/@type"/>
						<xsl:text>]</xsl:text>
					</xsl:when>
					<xsl:otherwise>
						<xsl:if test="name()='standard'">
							<xsl:choose>
								<xsl:when test="$ishtml='true'">
									<hr/>
								</xsl:when>
								<xsl:otherwise>
									<xsl:text>&#xA;</xsl:text>
								</xsl:otherwise>
							</xsl:choose>
						</xsl:if>
						<xsl:for-each select="*">
							<xsl:call-template name="temp">
								<xsl:with-param name="parent">
									<xsl:value-of select="$parent"/>.<xsl:value-of select="name()"/>
								</xsl:with-param>
							</xsl:call-template>
						</xsl:for-each>
						<xsl:if test="name()='standard'">
							<xsl:choose>
								<xsl:when test="$ishtml='true'">
									<hr/>
								</xsl:when>
								<xsl:otherwise>
									<xsl:text>&#xA;</xsl:text>
								</xsl:otherwise>
							</xsl:choose>
						</xsl:if>
					</xsl:otherwise>
				</xsl:choose>
				<xsl:if test="@type='cell' or @type='sparse' or @type='complex'">
					<xsl:choose>
						<xsl:when test="$ishtml='true'">
							<br/>
						</xsl:when>
						<xsl:otherwise>
							<xsl:text>&#xA;</xsl:text>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:if>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
