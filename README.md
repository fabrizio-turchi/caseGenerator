## caseGenerator,  tool for generating JSON files in UCO/CASE ontology.

**CASE** (see https://caseontology.org/index.html) stands for Cyber-investigation Analysis Standard Expression (CASE) and it is based on the Unified Cyber Ontology (UCO).

The open-source Cyber-investigation Analysis Standard Expression (CASE) is a community-developed ontology designed to serve as a standard for interchange, interoperability, and analysis of investigative information in a broad range of cyber-investigation domains, including digital forensic science, incident response, counter-terrorism, criminal justice, forensic intelligence, and situational awareness.
The necessity to conduct a depth study of the UCO/CASE standard has led to the development of a tool to generate the JSON-LD files corresponding to the standard representation of the above Use Cases. The result has been the creation of the caseGenerator application, an internal application for generating CASE evidence package in JSON-LD format. It’s a cross-platform application, it runs on Windows and OSX operating systems, and allows to generate all Objects provided by the standard language.
**Application Forms**
*Figure 1 (caseGenerator, main form)* shows the main form of the application, where besides the main data of the case there are all the buttons to generate each different kind of Object provided by the language:
> * Identity
> * Role
> * Relationship
> * Location
> * Tool
> * Warrant
> * Trace (Mobile device, SIM, Computer, File, Disk Partition, Phone account, Mobile Account, Email Account, Facebook Account, Message)
> * Provenance Record

![Figure 1: caseGenerator, main form](http://storeroom.igsg.cnr.it/caseGenerator_01.png)

*Figure 2 (caseGenerator, Identity form)* shows the information related to the Identity Object


![Figure 2: caseGenerator, Identity form](http://storeroom.igsg.cnr.it/caseGenerator_02.png)

*Figure 3 (caseGenerator, Mobile Device form)* represents the information related to the Mobile Device Object, while *Figure 4 (caseGenerator, Investigative Action form)* shows the InvestigativeAction Object and finally *Figure 5 (caseGenerator, Timeline related to the images extraction)* shows the timeline of the investigative case, related to the extraction of the images (Use Case D).


![Figure 3: caseGenerator, Mobile Device form](http://storeroom.igsg.cnr.it/caseGenerator_03.png)

![Figure 4: caseGenerator, Investigative Action form](http://storeroom.igsg.cnr.it/caseGenerator_04.png)

![Figure 5: caseGenerator, Timeline related to the images extraction](http://storeroom.igsg.cnr.it/caseGenerator_05.png)

**Processing flow**
The UCO/CASE Objects creation processing flow must act in accordance with the following order, because of some dependencies that have to be satisfied:
> 1.	Identity, it is independent Object
> 2.	Role, it is independent but a set of roles should be already defined that may include: Offender, Victim, Judge, D.F. Expert, LEA
> 3.	Relationship, it depends on Identity and Role
> 4.	Location, it is independent
> 5.	Warrant, it depends on Identity, Role and Relationship. There must be an Identity who plays the Role of Judge, so there must be an Identity tied/bound to a Relationship
> 6.	Tool, it is independent
> 7.	Trace, it is most independent in case like Mobile Device, SIM, File, DiskPartition, Mobile account, Email account, PhoneAccount, but Message depends on Phone Accont. A group of Trace should automatically generate some Relationship (SIM inside Smartphone, HD inside Computer, etc.)
> 8.	ProvenanceRecord, it depends on Trace and it should be automatically generated when a trace is defined
> 9.	Invesigative Action, it depends on ProvenanceRecord/Trace
